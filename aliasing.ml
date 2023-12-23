let flow_instruction_aliasing
    ((out_id : Ir.local_id), (instruction : Ir.instruction))
    (in_aliased : Ir.LocalIdSet.t) : Ir.LocalIdSet.t =
  let no_alias_out = Ir.LocalIdSet.remove out_id in_aliased in
  let alias_out = Ir.LocalIdSet.add out_id in_aliased in
  match instruction with
  | Alloc -> no_alias_out
  | GetGlobal _ -> alias_out
  | Load _load_id -> alias_out
  | Store (_store_id, store_val) -> Ir.LocalIdSet.add store_val no_alias_out
  | StoreEmptyTable _ -> no_alias_out
  | StoreClosure (_store_id, _closure_id, capture_ids) ->
      Ir.LocalIdSet.add_seq (List.to_seq capture_ids) no_alias_out
  | GetField _ -> alias_out
  | GetIndex _ -> alias_out
  | NumberConstant _ -> no_alias_out
  | BoolConstant _ -> no_alias_out
  | StringConstant _ -> no_alias_out
  | NilConstant -> no_alias_out
  | Call (_, arg_ids) -> Ir.LocalIdSet.add_seq (List.to_seq arg_ids) alias_out
  | UnaryOp _ -> no_alias_out
  | BinaryOp _ -> no_alias_out
  | Phi branches ->
      if
        branches
        |> List.find_opt (fun (_, id) -> Ir.LocalIdSet.mem id in_aliased)
        |> Option.is_some
      then Ir.LocalIdSet.add out_id no_alias_out
      else no_alias_out
  | DeleteLocal _ -> no_alias_out