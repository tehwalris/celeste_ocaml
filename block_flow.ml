type flow_node =
  | BeforeEntryBlock
  | AfterEntryBlock
  | BeforeNamedBlock of Ir.label
  | AfterNamedBlock of Ir.label
  | Return
[@@deriving show]

module G = Graph.Imperative.Digraph.Concrete (struct
  type t = flow_node

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end)

let targets_of_terminator (t : Ir.terminator) : flow_node list =
  match t with
  | Ir.Ret _ -> [ Return ]
  | Ir.Br l -> [ BeforeNamedBlock l ]
  | Ir.Cbr (_, l_true, l_false) ->
      [ BeforeNamedBlock l_true; BeforeNamedBlock l_false ]

let flow_graph_of_cfg (cfg : Ir.cfg) =
  let g = G.create () in

  G.add_vertex g BeforeEntryBlock;
  G.add_vertex g AfterEntryBlock;
  G.add_edge g BeforeEntryBlock AfterEntryBlock;

  G.add_vertex g Return;

  List.iter
    (fun (name, _block) ->
      G.add_vertex g (BeforeNamedBlock name);
      G.add_vertex g (AfterNamedBlock name);
      G.add_edge g (BeforeNamedBlock name) (AfterNamedBlock name))
    cfg.named;

  let add_block_terminator_edges after_block_node block =
    let _, terminator = block.Ir.terminator in
    terminator |> targets_of_terminator
    |> List.iter (fun target -> G.add_edge g after_block_node target)
  in

  add_block_terminator_edges AfterEntryBlock cfg.entry;
  List.iter
    (fun (name, block) ->
      add_block_terminator_edges (AfterNamedBlock name) block)
    cfg.named;

  g

let make_flow_function (flow_block_phi : Ir.label -> Ir.block -> 'a -> 'a)
    (flow_block_before_join : Ir.block -> 'a -> 'a)
    (flow_block_post_phi : Ir.block -> 'a -> 'a)
    (flow_branch : Ir.terminator -> Ir.label -> 'a -> 'a)
    (flow_return : Ir.terminator -> 'a -> 'a) (cfg : Ir.cfg) :
    G.E.t -> 'a option -> 'a option =
  let named_blocks =
    cfg.named |> List.to_seq
    |> Seq.map (fun (k, v) -> (k, (v, 0)))
    |> Ir.LabelMap.of_seq
  in
  let named_blocks =
    let target_labels =
      List.concat_map
        (fun source_block ->
          match source_block.Ir.terminator with
          | _, Ret _ -> []
          | _, Br l -> [ l ]
          | _, Cbr (_, l_true, l_false) -> [ l_true; l_false ])
        (cfg.entry :: List.map snd cfg.named)
    in
    List.fold_left
      (fun named_blocks l ->
        let b, d = Ir.LabelMap.find l named_blocks in
        Ir.LabelMap.add l (b, d + 1) named_blocks)
      named_blocks target_labels
  in
  let noop_if_degree_1 in_degree f =
    assert (in_degree > 0);
    fun v -> if in_degree > 1 then f v else v
  in
  let flow_some edge =
    match edge with
    | BeforeEntryBlock, AfterEntryBlock -> flow_block_post_phi cfg.entry
    | BeforeNamedBlock name, AfterNamedBlock other_name when name = other_name
      ->
        flow_block_post_phi (fst @@ Ir.LabelMap.find name named_blocks)
    | AfterEntryBlock, BeforeNamedBlock target_name ->
        let _, terminator = cfg.entry.terminator in
        let target_block, target_block_in_degree =
          Ir.LabelMap.find target_name named_blocks
        in
        assert (target_block_in_degree > 0);
        fun v ->
          v
          |> flow_branch terminator target_name
          |> noop_if_degree_1 target_block_in_degree
               (flow_block_before_join target_block)
    | AfterNamedBlock source_name, BeforeNamedBlock target_name ->
        let source_block, _ = Ir.LabelMap.find source_name named_blocks in
        let target_block, target_block_in_degree =
          Ir.LabelMap.find target_name named_blocks
        in
        let _, terminator = source_block.terminator in
        fun v ->
          v
          |> flow_branch terminator target_name
          |> flow_block_phi source_name target_block
          |> noop_if_degree_1 target_block_in_degree
               (flow_block_before_join target_block)
    | AfterEntryBlock, Return ->
        let _, terminator = cfg.entry.terminator in
        flow_return terminator
    | AfterNamedBlock source_name, Return ->
        let source_block, _ = Ir.LabelMap.find source_name named_blocks in
        let _, terminator = source_block.terminator in
        flow_return terminator
    | _ -> failwith "flow has unexpected edge"
  in
  let flow_option edge = Option.map @@ flow_some edge in
  flow_option