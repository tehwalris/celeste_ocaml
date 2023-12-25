open Ir

module G = Graph.Imperative.Digraph.Concrete (struct
  type t = label

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end)

let rec contract_blocks (cfg : cfg) : cfg =
  let g = G.create () in
  List.iter (fun (name, _) -> G.add_vertex g name) cfg.named;
  List.iter
    (fun (name, block) ->
      match block.terminator with
      | _, Ret _ -> ()
      | _, Br target_name -> G.add_edge g name target_name
      | _, Cbr (_, true_name, false_name) ->
          G.add_edge g name true_name;
          G.add_edge g name false_name)
    cfg.named;

  let edge_to_contract =
    G.fold_edges
      (fun a b found_edge ->
        match found_edge with
        | Some _ -> found_edge
        | None when G.out_degree g a = 1 && G.in_degree g b = 1 -> Some (a, b)
        | _ -> None)
      g None
  in

  match edge_to_contract with
  | Some (name_a, name_b) ->
      let block_a = List.assoc name_a cfg.named in
      (match block_a.terminator with _, Br _ -> () | _ -> assert false);
      let block_b = List.assoc name_b cfg.named in
      let block_b_phi, _ = split_block_phi_instructions block_b in
      assert (block_b_phi = []);
      let new_block_b =
        {
          block_b with
          instructions =
            List.concat [ block_a.instructions; block_b.instructions ];
        }
      in
      let cfg =
        {
          cfg with
          named =
            List.filter_map
              (fun (name, block) ->
                if name = name_a then None
                else if name = name_b then Some (name, new_block_b)
                else Some (name, block))
              cfg.named;
        }
      in
      let adjust_branch_target name = if name = name_a then name_b else name in
      let cfg =
        cfg_map_blocks
          (fun block ->
            let terminator =
              match block.terminator with
              | id, Br target_name -> (id, Br (adjust_branch_target target_name))
              | id, Cbr (value, true_name, false_name) ->
                  ( id,
                    Cbr
                      ( value,
                        adjust_branch_target true_name,
                        adjust_branch_target false_name ) )
              | terminator -> terminator
            in
            { block with terminator })
          cfg
      in
      contract_blocks cfg
  | None -> cfg