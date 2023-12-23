let flow_instruction_live_variables
    ((out_id : Ir.local_id), (instruction : Ir.instruction))
    (in_live : Ir.LocalIdSet.t) : Ir.LocalIdSet.t =
  let arg_ids = ref [] in
  (match instruction with
  | Ir.DeleteLocal _ -> ()
  | _ ->
      ignore
      @@ Ir.instruction_map_local_ids
           (fun id ->
             arg_ids := id :: !arg_ids;
             id)
           instruction);
  let arg_ids = !arg_ids in
  in_live
  |> Ir.LocalIdSet.remove out_id
  |> Ir.LocalIdSet.add_seq (List.to_seq arg_ids)

let flow_terminator_live_variables (terminator : Ir.terminator)
    (in_live : Ir.LocalIdSet.t) : Ir.LocalIdSet.t =
  let arg_ids = ref [] in
  let _ =
    Ir.terminator_map_local_ids
      (fun id ->
        arg_ids := id :: !arg_ids;
        id)
      terminator
  in
  let arg_ids = !arg_ids in
  in_live |> Ir.LocalIdSet.add_seq (List.to_seq arg_ids)