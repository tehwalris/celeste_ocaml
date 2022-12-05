type flow_side = Before | After

module G = Graph.Imperative.Digraph.Concrete (struct
  type t = flow_side * Ir.local_id

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end)

module FlowGraphDot = Graph.Graphviz.Dot (struct
  include G

  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [ `Shape `Box ]

  let vertex_name (side, id) =
    let side_string = match side with Before -> "Before" | After -> "After" in
    Printf.sprintf "\"(%s, %d)\"" side_string id

  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let entry_vertex_of_block (block : Ir.block) : G.V.t =
  match block.instructions with
  | (id, _) :: _ -> (Before, id)
  | [] -> (Before, fst @@ block.terminator)

let targets_of_terminator (t : Ir.terminator) : Ir.label list =
  match t with
  | Ir.Ret _ -> []
  | Ir.Br l -> [ l ]
  | Ir.Cbr (_, l_true, l_false) -> [ l_true; l_false ]

let drop_last l = l |> List.rev |> List.tl |> List.rev

let flow_graph_of_cfg (cfg : Ir.cfg) =
  let entry_vertex_by_block_label =
    List.map
      (fun (label, block) -> (label, entry_vertex_of_block block))
      cfg.named
  in
  let g = G.create () in
  List.iter
    (fun (block : Ir.block) ->
      let ids = List.map fst block.instructions @ [ fst block.terminator ] in
      assert (ids <> []);
      let prev_ids = None :: (ids |> drop_last |> List.map (fun v -> Some v)) in
      List.iter2
        (fun id prev_id ->
          G.add_vertex g (Before, id);
          G.add_vertex g (After, id);
          G.add_edge g (Before, id) (After, id);
          match prev_id with
          | Some prev_id -> G.add_edge g (After, prev_id) (Before, id)
          | _ -> ())
        ids prev_ids;
      block.terminator |> snd |> targets_of_terminator
      |> List.iter (fun label ->
             let side, id = List.assoc label entry_vertex_by_block_label in
             assert (side = Before);
             G.add_edge g (After, fst block.terminator) (Before, id));
      ())
    (cfg.entry :: List.map snd cfg.named);
  g

let make_flow_function
    (instruction_flow : Ir.local_id * Ir.instruction -> 'a -> 'a)
    (terminator_flow : Ir.terminator -> 'a -> 'a) (cfg : Ir.cfg) :
    G.E.t -> 'a option -> 'a option =
  let blocks = cfg.entry :: List.map snd cfg.named in
  let instruction_flow_functions =
    List.concat_map
      (fun (block : Ir.block) ->
        List.map
          (fun (id, instruction) -> (id, instruction_flow (id, instruction)))
          block.instructions)
      blocks
  in
  let terminator_flow_functions =
    List.map
      (fun (block : Ir.block) ->
        (fst block.terminator, terminator_flow @@ snd block.terminator))
      blocks
  in
  let flow_functions =
    Seq.append
      (List.to_seq instruction_flow_functions)
      (List.to_seq terminator_flow_functions)
    |> Ir.LocalIdMap.of_seq
  in
  fun edge ->
    match edge with
    | (Before, in_id), (After, out_id) when in_id = out_id ->
        Option.map @@ Ir.LocalIdMap.find in_id flow_functions
    | (After, _), (Before, _) -> Fun.id
    | _ -> failwith "flow through node went in unexpected direction"

let lift_join (join : 'a -> 'a -> 'a) (a : 'a option) (b : 'a option) :
    'a option =
  match (a, b) with
  | Some a, Some b -> Some (join a b)
  | Some a, None -> Some a
  | None, Some b -> Some b
  | None, None -> None

let lift_equal (equal : 'a -> 'a -> bool) (a : 'a option) (b : 'a option) : bool
    =
  match (a, b) with
  | Some a, Some b -> equal a b
  | Some _, None -> false
  | None, Some _ -> false
  | None, None -> true