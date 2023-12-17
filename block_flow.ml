type flow_node =
  | BeforeEntryBlock
  | AfterEntryBlock
  | BeforeNamedBlock of Ir.label
  | AfterNamedBlock of Ir.label
  | Return

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

let make_flow_function (block_flow : Ir.block -> 'a -> 'a)
    (terminator_branch_flow : Ir.terminator -> Ir.label -> 'a -> 'a)
    (terminator_return_flow : Ir.terminator -> 'a -> 'a) (cfg : Ir.cfg) :
    G.E.t -> 'a option -> 'a option =
  let named_blocks = cfg.named |> List.to_seq |> Ir.LabelMap.of_seq in
  let flow_some edge =
    match edge with
    | BeforeEntryBlock, AfterEntryBlock -> block_flow cfg.entry
    | BeforeNamedBlock name, AfterNamedBlock other_name when name = other_name
      ->
        block_flow (Ir.LabelMap.find name named_blocks)
    | AfterEntryBlock, BeforeNamedBlock target_name ->
        let _, terminator = cfg.entry.terminator in
        terminator_branch_flow terminator target_name
    | AfterNamedBlock source_name, BeforeNamedBlock target_name ->
        let source_block = Ir.LabelMap.find source_name named_blocks in
        let _, terminator = source_block.terminator in
        terminator_branch_flow terminator target_name
    | AfterEntryBlock, Return ->
        let _, terminator = cfg.entry.terminator in
        terminator_return_flow terminator
    | AfterNamedBlock source_name, Return ->
        let source_block = Ir.LabelMap.find source_name named_blocks in
        let _, terminator = source_block.terminator in
        terminator_return_flow terminator
    | _ -> failwith "flow has unexpected edge"
  in
  let flow_option edge = Option.map @@ flow_some edge in
  flow_option