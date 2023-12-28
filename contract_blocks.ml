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
      let new_block_a =
        {
          instructions =
            List.concat [ block_a.instructions; block_b.instructions ];
          terminator = block_b.terminator;
          hint_normalize = block_a.hint_normalize || block_b.hint_normalize;
        }
      in
      let cfg =
        {
          cfg with
          named =
            List.filter_map
              (fun (name, block) ->
                if name = name_a then Some (name, new_block_a)
                else if name = name_b then None
                else Some (name, block))
              cfg.named;
        }
        |> cfg_map_blocks (fun block ->
               {
                 block with
                 instructions =
                   List.map
                     (function
                       | insn_id, Phi l ->
                           ( insn_id,
                             Phi
                               (List.map
                                  (fun (name, v) ->
                                    if name = name_b then (name_a, v)
                                    else (name, v))
                                  l) )
                       | instr -> instr)
                     block.instructions;
               })
      in
      contract_blocks cfg
  | None -> cfg