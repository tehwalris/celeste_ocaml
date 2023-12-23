open Flow

type fact = {
  potentially_aliased : Ir.LocalIdSet.t;
  known_stores : Ir.local_id Ir.LocalIdMap.t;
  replacements : Ir.local_id Ir.LocalIdMap.t;
}

let show_fact (fact : fact) : string =
  Printf.sprintf
    "{ potentially_aliased: [ %s ]; known_store = %s; replacements = [ %s ] }"
    (Ir.show_local_id_set fact.potentially_aliased)
    (Ir.LocalIdMap.bindings fact.known_stores
    |> List.map (fun (a, b) ->
           Printf.sprintf "(%s, %s)" (Ir.show_local_id a) (Ir.show_local_id b))
    |> String.concat "; ")
    (Ir.LocalIdMap.bindings fact.replacements
    |> List.map (fun (a, b) ->
           Printf.sprintf "(%s, %s)" (Ir.show_local_id a) (Ir.show_local_id b))
    |> String.concat "; ")

let known_stores_join =
  let union_same_values =
    Ir.LocalIdMap.union (fun _ a b -> if a == b then Some a else None)
  in
  lift_join (fun a b ->
      {
        potentially_aliased =
          Ir.LocalIdSet.union a.potentially_aliased b.potentially_aliased;
        known_stores = union_same_values a.known_stores b.known_stores;
        replacements = union_same_values a.replacements b.replacements;
      })

let known_stores_equal =
  lift_equal (fun a b ->
      Ir.LocalIdSet.equal a.potentially_aliased b.potentially_aliased
      && Ir.LocalIdMap.equal (fun _ _ -> true) a.known_stores b.known_stores
      && Ir.LocalIdMap.equal (fun _ _ -> true) a.replacements b.replacements)

let flow_function_of_cfg =
  Flow.make_flow_function
    (fun (out_id, instruction) in_fact ->
      let base_out_fact =
        {
          in_fact with
          potentially_aliased =
            Aliasing.flow_instruction_aliasing (out_id, instruction)
              in_fact.potentially_aliased;
          replacements = Ir.LocalIdMap.remove out_id in_fact.replacements;
        }
      in
      let remove_potentially_aliased_stores
          (known_stores : Ir.local_id Ir.LocalIdMap.t) =
        Ir.LocalIdMap.filter
          (fun id _ ->
            not (Ir.LocalIdSet.mem id base_out_fact.potentially_aliased))
          known_stores
      in
      match instruction with
      | Alloc -> base_out_fact
      | GetGlobal _ -> base_out_fact
      | Load load_id ->
          {
            base_out_fact with
            replacements =
              (match Ir.LocalIdMap.find_opt load_id in_fact.known_stores with
              | Some store_val ->
                  Ir.LocalIdMap.add out_id store_val base_out_fact.replacements
              | _ -> base_out_fact.replacements);
          }
      | Store (store_id, store_val) ->
          {
            base_out_fact with
            known_stores =
              base_out_fact.known_stores
              |> Ir.LocalIdMap.add store_id store_val
              |> remove_potentially_aliased_stores;
          }
      | StoreEmptyTable _ ->
          {
            base_out_fact with
            known_stores =
              remove_potentially_aliased_stores base_out_fact.known_stores;
          }
      | StoreClosure _ ->
          {
            base_out_fact with
            known_stores =
              remove_potentially_aliased_stores base_out_fact.known_stores;
          }
      | GetField _ -> base_out_fact
      | GetIndex _ -> base_out_fact
      | NumberConstant _ -> base_out_fact
      | BoolConstant _ -> base_out_fact
      | StringConstant _ -> base_out_fact
      | NilConstant -> base_out_fact
      | Call (_, _arg_ids) ->
          {
            base_out_fact with
            known_stores =
              remove_potentially_aliased_stores base_out_fact.known_stores;
          }
      | UnaryOp _ -> base_out_fact
      | BinaryOp _ -> base_out_fact
      | Phi _branches ->
          {
            (* it's probably possible to preserve more information here *)
            base_out_fact
            with
            known_stores =
              remove_potentially_aliased_stores base_out_fact.known_stores;
          }
      | DeleteLocal _ -> base_out_fact)
    (fun _terminator in_fact -> in_fact)

let bypass_redundant_loads (cfg : Ir.cfg) : Ir.cfg =
  let module FlowAnalysis =
    Graph.Fixpoint.Make
      (G)
      (struct
        type vertex = G.E.vertex
        type edge = G.E.t
        type g = G.t
        type data = fact option

        let direction = Graph.Fixpoint.Forward
        let equal = known_stores_equal
        let join = known_stores_join
        let analyze = flow_function_of_cfg cfg
      end)
  in
  let g = flow_graph_of_cfg cfg in
  let init v =
    if G.in_degree g v = 0 then
      Some
        {
          (* TODO function parameters may be aliased *)
          potentially_aliased = Ir.LocalIdSet.empty;
          known_stores = Ir.LocalIdMap.empty;
          replacements = Ir.LocalIdMap.empty;
        }
    else None
  in
  let get_fact = FlowAnalysis.analyze init g in
  let get_arg_mapping instruction_id =
    let in_fact = Option.get @@ get_fact (Before, instruction_id) in
    fun id ->
      Ir.LocalIdMap.find_opt id in_fact.replacements |> Option.value ~default:id
  in
  let convert_block (block : Ir.block) : Ir.block =
    {
      instructions =
        List.map
          (fun (instruction_id, instruction) ->
            ( instruction_id,
              Ir.instruction_map_local_ids
                (get_arg_mapping instruction_id)
                instruction ))
          block.instructions;
      terminator =
        (let terminator_id, terminator = block.terminator in
         ( terminator_id,
           Ir.terminator_map_local_ids
             (get_arg_mapping terminator_id)
             terminator ));
    }
  in
  Ir.cfg_map_blocks convert_block cfg
