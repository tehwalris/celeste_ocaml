open Flow

let instruction_live_if_output_unused (instruction : Ir.instruction)
    (aliased_variables : Ir.LocalIdSet.t) : bool =
  match instruction with
  | Alloc -> false
  | GetGlobal _ -> false
  | Load _ -> false
  | Store (store_id, _) -> Ir.LocalIdSet.mem store_id aliased_variables
  | StoreEmptyTable store_id -> Ir.LocalIdSet.mem store_id aliased_variables
  | StoreClosure (store_id, _closure_id, _capture_ids) ->
      Ir.LocalIdSet.mem store_id aliased_variables
  | GetField (_, _, create_if_missing) -> create_if_missing
  | GetIndex (_, _, create_if_missing) -> create_if_missing
  | NumberConstant _ -> false
  | BoolConstant _ -> false
  | StringConstant _ -> false
  | NilConstant -> false
  | Call _ -> true
  | UnaryOp _ -> false
  | BinaryOp _ -> false
  | Phi _ -> false
  | DeleteLocal _ -> true

let remove_dead_instructions (cfg : Ir.cfg) : Ir.cfg =
  let module AliasAnalysis =
    Graph.Fixpoint.Make
      (G)
      (struct
        type vertex = G.E.vertex
        type edge = G.E.t
        type g = G.t
        type data = Ir.LocalIdSet.t option

        let direction = Graph.Fixpoint.Forward
        let equal = lift_equal Ir.LocalIdSet.equal
        let join = lift_join Ir.LocalIdSet.union

        let analyze =
          make_flow_function Aliasing.flow_instruction_aliasing
            (fun _ in_fact -> in_fact)
            cfg
      end)
  in
  let module LiveVariableAnalysis =
    Graph.Fixpoint.Make
      (G)
      (struct
        type vertex = G.E.vertex
        type edge = G.E.t
        type g = G.t
        type data = Ir.LocalIdSet.t option

        let direction = Graph.Fixpoint.Backward
        let equal = lift_equal Ir.LocalIdSet.equal
        let join = lift_join Ir.LocalIdSet.union

        let analyze =
          make_flow_function Liveness.flow_instruction_live_variables
            Liveness.flow_terminator_live_variables cfg
      end)
  in
  let g = flow_graph_of_cfg cfg in
  let alias_analysis =
    (* TODO function parameters may be aliased *)
    AliasAnalysis.analyze (fun _ -> Some Ir.LocalIdSet.empty) g
  in
  let ever_aliased_variables =
    G.fold_vertex
      (fun v -> Ir.LocalIdSet.union @@ Option.get @@ alias_analysis v)
      g Ir.LocalIdSet.empty
  in
  let live_variable_analysis =
    LiveVariableAnalysis.analyze (fun _ -> Some Ir.LocalIdSet.empty) g
  in
  let instruction_is_live (out_id, instruction) =
    let live_variables = Option.get @@ live_variable_analysis (After, out_id) in
    instruction_live_if_output_unused instruction
      (Ir.LocalIdSet.union live_variables ever_aliased_variables)
    || Ir.LocalIdSet.mem out_id live_variables
  in
  Ir.cfg_map_blocks
    (fun (block : Ir.block) ->
      {
        block with
        instructions = List.filter instruction_is_live block.instructions;
      })
    cfg