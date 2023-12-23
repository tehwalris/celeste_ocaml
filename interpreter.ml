type heap_id = int [@@deriving show]

let gen_heap_id : unit -> heap_id =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

type value =
  | VNumber of Pico_number.t
  | VBool of bool
  | VUnknownBool
  | VString of string
  | VNil
  | VPointer of heap_id
[@@deriving show]

type heap_value =
  | HValue of value
  | HObjectTable of (string * heap_id) list
  | HArrayTable of heap_id list
  | HUnknownTable
  | HClosure of Ir.global_id * value list
  | HBuiltinFun of string

module HeapIdMap = Map.Make (struct
  type t = heap_id

  let compare = Stdlib.compare
end)

module StringMap = Map.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

type state = {
  heap : heap_value HeapIdMap.t;
  local_env : value Ir.LocalIdMap.t;
  global_env : heap_id StringMap.t;
  prints : string list;
}

type builtin_fun = state -> value list -> state * value

type fixed_env = {
  fun_defs : Ir.fun_def list;
  builtin_funs : (string * builtin_fun) list;
}

let heap_id_from_pointer_local (state : state) (local_id : Ir.local_id) :
    heap_id =
  match Ir.LocalIdMap.find local_id state.local_env with
  | VPointer heap_id -> heap_id
  | _ -> failwith "Value is not a pointer"

let state_heap_add (state : state) (heap_value : heap_value) : state * heap_id =
  let heap_id = gen_heap_id () in
  let state =
    { state with heap = HeapIdMap.add heap_id heap_value state.heap }
  in
  (state, heap_id)

let state_heap_update (state : state) (update : heap_value -> heap_value)
    (heap_id : heap_id) : state =
  let old_heap_value = HeapIdMap.find heap_id state.heap in
  let new_heap_value = update old_heap_value in
  { state with heap = HeapIdMap.add heap_id new_heap_value state.heap }

module StateSet = Set.Make (struct
  type t = state

  let compare = Stdlib.compare
end)

module StateAndReturnSet = Set.Make (struct
  type t = state * value

  let compare = Stdlib.compare
end)

module StateAndMaybeReturnSet = struct
  type t = StateSet of StateSet.t | StateAndReturnSet of StateAndReturnSet.t

  let union (a : t) (b : t) : t =
    match (a, b) with
    | StateSet a, StateSet b -> StateSet (StateSet.union a b)
    | StateAndReturnSet a, StateAndReturnSet b ->
        StateAndReturnSet (StateAndReturnSet.union a b)
    | _ -> failwith "Cannot union StateSet and StateAndReturnSet"

  let equal (a : t) (b : t) : bool =
    match (a, b) with
    | StateSet a, StateSet b -> StateSet.equal a b
    | StateAndReturnSet a, StateAndReturnSet b -> StateAndReturnSet.equal a b
    | _ -> false
end

type terminator_result =
  (* each item corresponds to an input state *)
  | Ret of value list option
  (* each item might _not_ correspond to an input state *)
  | Br of (Ir.label * state) list

let interpret_unary_op (op : string) (v : value) : value =
  match (op, v) with
  | "-", VNumber v -> VNumber (Pico_number.neg v)
  | "not", VBool v -> VBool (not v)
  | "not", VUnknownBool -> VUnknownBool
  | op, v ->
      failwith @@ Printf.sprintf "Unsupported unary op: %s %s" op (show_value v)

let interpret_binary_op (l : value) (op : string) (r : value) : value =
  match (l, op, r) with
  | VNumber l, "+", VNumber r -> VNumber (Pico_number.add l r)
  | VNumber l, "-", VNumber r -> VNumber (Pico_number.sub l r)
  | VNumber l, "*", VNumber r -> VNumber (Pico_number.mul l r)
  | VNumber l, "/", VNumber r -> VNumber (Pico_number.div l r)
  | VNumber l, "%", VNumber r -> VNumber (Pico_number.modulo l r)
  | VNumber l, "<", VNumber r -> VBool (Int32.compare l r < 0)
  | VNumber l, "<=", VNumber r -> VBool (Int32.compare l r <= 0)
  | VNumber l, ">", VNumber r -> VBool (Int32.compare l r > 0)
  | VNumber l, ">=", VNumber r -> VBool (Int32.compare l r >= 0)
  | VNumber l, "==", VNumber r -> VBool (Int32.compare l r = 0)
  | VNumber l, "~=", VNumber r -> VBool (Int32.compare l r <> 0)
  | VString l, "..", VString r -> VString (l ^ r)
  | VString l, "..", VNumber r ->
      VString (l ^ Int.to_string @@ Pico_number.int_of r)
  | VNumber l, "..", VString r ->
      VString ((Int.to_string @@ Pico_number.int_of l) ^ r)
  | VString l, "==", VString r -> VBool (String.equal l r)
  | l, op, r ->
      failwith
      @@ Printf.sprintf "Unsupported binary op: %s %s %s" (show_value l) op
           (show_value r)

let rec interpret_instruction (fixed_env : fixed_env) (states : state list)
    (source_block_for_phi : Ir.label option) (insn : Ir.instruction) :
    (state * value) list * Ir.label option =
  let handle_separately_no_phi (f : state -> state * value) :
      (state * value) list * Ir.label option =
    (List.map f states, None)
  in
  match insn with
  | Alloc ->
      handle_separately_no_phi (fun state ->
          let heap_id = gen_heap_id () in
          let state =
            { state with heap = HeapIdMap.add heap_id (HValue VNil) state.heap }
          in
          (state, VPointer heap_id))
  | GetGlobal (name, create_if_missing) ->
      handle_separately_no_phi (fun state ->
          let heap_id, state =
            match StringMap.find_opt name state.global_env with
            | Some heap_id -> (heap_id, state)
            | None ->
                if not create_if_missing then
                  failwith
                  @@ Printf.sprintf
                       "Global %s not found, but create_if_missing was false"
                       name;
                let state, heap_id = state_heap_add state (HValue VNil) in
                let state =
                  {
                    state with
                    global_env = StringMap.add name heap_id state.global_env;
                  }
                in
                (heap_id, state)
          in
          (state, VPointer heap_id))
  | Load local_id ->
      handle_separately_no_phi (fun state ->
          let heap_id = heap_id_from_pointer_local state local_id in
          let value =
            match HeapIdMap.find heap_id state.heap with
            | HValue value -> value
            | _ ->
                failwith "Value is of a type that can not be stored in a local"
          in
          (state, value))
  | Store (target_local_id, source_local_id) ->
      handle_separately_no_phi (fun state ->
          let heap_id = heap_id_from_pointer_local state target_local_id in
          let source_value =
            Ir.LocalIdMap.find source_local_id state.local_env
          in
          let state =
            state_heap_update state (fun _ -> HValue source_value) heap_id
          in
          (state, VNil))
  | StoreEmptyTable local_id ->
      handle_separately_no_phi (fun state ->
          let heap_id = heap_id_from_pointer_local state local_id in
          let state =
            state_heap_update state (fun _ -> HObjectTable []) heap_id
          in
          (state, VNil))
  | StoreClosure (target_local_id, fun_global_id, captured_local_ids) ->
      handle_separately_no_phi (fun state ->
          let heap_id = heap_id_from_pointer_local state target_local_id in
          let captured_values =
            List.map
              (fun id -> Ir.LocalIdMap.find id state.local_env)
              captured_local_ids
          in
          let state =
            state_heap_update state
              (fun _ -> HClosure (fun_global_id, captured_values))
              heap_id
          in
          (state, VNil))
  | GetField (table_local_id, field_name, create_if_missing) ->
      handle_separately_no_phi (fun state ->
          let table_heap_id = heap_id_from_pointer_local state table_local_id in
          let old_fields =
            match HeapIdMap.find table_heap_id state.heap with
            | HObjectTable old_fields -> old_fields
            | HUnknownTable -> []
            | _ ->
                failwith
                  "GetField called on something that's not an object-like \
                   table or unknown table"
          in
          let state, field_heap_id =
            match List.assoc_opt field_name old_fields with
            | Some field_heap_id -> (state, field_heap_id)
            | None ->
                if not create_if_missing then
                  failwith "Field not found, but create_if_missing was false";
                let state, field_heap_id = state_heap_add state (HValue VNil) in
                let state =
                  state_heap_update state
                    (fun _ ->
                      HObjectTable ((field_name, field_heap_id) :: old_fields))
                    table_heap_id
                in
                (state, field_heap_id)
          in
          (state, VPointer field_heap_id))
  | GetIndex (table_local_id, index_local_id, create_if_missing) ->
      handle_separately_no_phi (fun state ->
          let table_heap_id = heap_id_from_pointer_local state table_local_id in
          let index =
            match Ir.LocalIdMap.find index_local_id state.local_env with
            | VNumber i ->
                if Pico_number.fraction_int_of i <> 0 then
                  failwith "Index is not an integer";
                Pico_number.int_of i
            | _ -> failwith "Index is not an number"
          in
          let old_fields =
            match HeapIdMap.find table_heap_id state.heap with
            | HArrayTable old_fields -> old_fields
            | HUnknownTable -> []
            | _ ->
                failwith
                  "GetIndex called on something that's not an array-like table \
                   or unknown table"
          in
          let state, field_heap_id =
            match List.nth_opt old_fields index with
            | Some field_heap_id -> (state, field_heap_id)
            | None ->
                if not create_if_missing then
                  failwith "Index not found, but create_if_missing was false";
                let state, field_heap_id = state_heap_add state (HValue VNil) in
                let state =
                  state_heap_update state
                    (fun _ -> HArrayTable (old_fields @ [ field_heap_id ]))
                    table_heap_id
                in
                (state, field_heap_id)
          in
          (state, VPointer field_heap_id))
  | NumberConstant v ->
      handle_separately_no_phi (fun state -> (state, VNumber v))
  | BoolConstant v -> handle_separately_no_phi (fun state -> (state, VBool v))
  | StringConstant v ->
      handle_separately_no_phi (fun state -> (state, VString v))
  | NilConstant -> handle_separately_no_phi (fun state -> (state, VNil))
  | Call (fun_local_id, arg_local_ids) ->
      let results =
        List.concat_map
          (fun state ->
            let arg_values =
              List.map
                (fun id -> Ir.LocalIdMap.find id state.local_env)
                arg_local_ids
            in
            let fun_heap_id = heap_id_from_pointer_local state fun_local_id in
            match HeapIdMap.find fun_heap_id state.heap with
            | HBuiltinFun name ->
                let builtin_fun = List.assoc name fixed_env.builtin_funs in
                [ builtin_fun state arg_values ]
            | HClosure (fun_global_id, captured_values) ->
                let fun_def =
                  List.find
                    (fun def -> def.Ir.name = fun_global_id)
                    fixed_env.fun_defs
                in
                let inner_state : state =
                  {
                    state with
                    local_env =
                      List.fold_left2
                        (fun locals id value ->
                          Ir.LocalIdMap.add id value locals)
                        Ir.LocalIdMap.empty
                        (fun_def.Ir.capture_ids @ fun_def.Ir.arg_ids)
                        (captured_values @ arg_values);
                  }
                in
                interpret_cfg_flow fixed_env
                  (StateSet.singleton inner_state)
                  fun_def.cfg
                |> (function
                     | StateAndMaybeReturnSet.StateSet inner_states ->
                         inner_states |> StateSet.to_seq
                         |> Seq.map (fun inner_state -> (inner_state, VNil))
                     | StateAndMaybeReturnSet.StateAndReturnSet
                         inner_states_and_returns ->
                         inner_states_and_returns |> StateAndReturnSet.to_seq)
                |> Seq.map (fun (inner_state, return_value) ->
                       let state =
                         {
                           heap = inner_state.heap;
                           local_env = state.local_env;
                           global_env = inner_state.global_env;
                           prints = inner_state.prints;
                         }
                       in
                       (state, return_value))
                |> List.of_seq
            | _ -> failwith "Calling something that's not a function")
          states
      in
      (results, None)
  | UnaryOp (op, local_id) ->
      handle_separately_no_phi (fun state ->
          let value = Ir.LocalIdMap.find local_id state.local_env in
          (state, interpret_unary_op op value))
  | BinaryOp (left_local_id, op, right_local_id) ->
      handle_separately_no_phi (fun state ->
          let left_value = Ir.LocalIdMap.find left_local_id state.local_env in
          let right_value = Ir.LocalIdMap.find right_local_id state.local_env in
          (state, interpret_binary_op left_value op right_value))
  (* TODO remove source_block_for_phi once there's only the flow-based interpreter *)
  | Phi local_ids_by_label ->
      let results =
        List.map
          (fun state ->
            let local_id =
              List.assoc (Option.get source_block_for_phi) local_ids_by_label
            in
            let value = Ir.LocalIdMap.find local_id state.local_env in
            (state, value))
          states
      in
      (results, source_block_for_phi)

and interpret_terminator (states : state list) (terminator : Ir.terminator) :
    terminator_result =
  match terminator with
  | Ir.Ret (Some local_id) ->
      let return_values =
        List.map
          (fun state -> Ir.LocalIdMap.find local_id state.local_env)
          states
      in
      Ret (Some return_values)
  | Ir.Ret None -> Ret None
  | Ir.Br label -> Br (List.map (fun state -> (label, state)) states)
  | Ir.Cbr (local_id, true_label, false_label) ->
      Br
        (List.concat_map
           (fun state ->
             let labels =
               match Ir.LocalIdMap.find local_id state.local_env with
               | VBool true -> [ true_label ]
               | VBool false -> [ false_label ]
               | VUnknownBool -> [ true_label; false_label ]
               | _ -> failwith "Cbr called on non-boolean value"
             in
             List.map (fun label -> (label, state)) labels)
           states)

and interpret_block_single_state (fixed_env : fixed_env) (state : state)
    (source_block_for_phi : Ir.label option) (block : Ir.block) :
    state * terminator_result =
  let state, _ =
    List.fold_left
      (fun (state, source_block_for_phi) (local_id, insn) ->
        let results, source_block_for_phi =
          interpret_instruction fixed_env [ state ] source_block_for_phi insn
        in
        if List.length results <> 1 then
          failwith
            "State branched while interpreting block in single state mode";
        let state, value = List.hd results in
        let state =
          {
            state with
            local_env = Ir.LocalIdMap.add local_id value state.local_env;
          }
        in
        (state, source_block_for_phi))
      (state, source_block_for_phi)
      block.instructions
  in
  let _, terminator = block.terminator in
  let terminator_result = interpret_terminator [ state ] terminator in
  (state, terminator_result)

and interpret_cfg_single_state (fixed_env : fixed_env) (state : state)
    (cfg : Ir.cfg) : state * value option =
  let rec interpret_block_rec state source_block_for_phi block_label block =
    let state, terminator_result =
      interpret_block_single_state fixed_env state source_block_for_phi block
    in
    match terminator_result with
    | Ret (Some [ value ]) -> (state, Some value)
    | Ret (Some l) ->
        failwith
        @@ Printf.sprintf "Ret contained %d values, expected 1"
        @@ List.length l
    | Ret None -> (state, None)
    | Br [ (label, state) ] ->
        interpret_block_rec state block_label (Some label)
        @@ List.assoc label cfg.named
    | Br _ -> failwith "Br returned multiple states"
  in
  interpret_block_rec state None None cfg.entry

and split_block_phi_instructions (block : Ir.block) =
  let is_phi = function _, Ir.Phi _ -> true | _ -> false in
  let unwrap_phi = function
    | id, Ir.Phi v -> (id, v)
    | _ -> failwith "Not a phi instruction"
  in
  let phi_instructions, non_phi_instructions =
    BatList.span is_phi block.instructions
  in
  let phi_instructions = List.map unwrap_phi phi_instructions in
  if List.exists is_phi non_phi_instructions then
    failwith "Phi instructions are not at the beginning of the block";
  (phi_instructions, non_phi_instructions)

and phi_block_flow (source_block_name : Ir.label) (target_block : Ir.block)
    (states : StateSet.t) : StateSet.t =
  let phi_instructions, _ = split_block_phi_instructions target_block in
  StateSet.map
    (fun state ->
      List.fold_left
        (fun state (local_id, local_ids_by_label) ->
          let value =
            Ir.LocalIdMap.find
              (List.assoc source_block_name local_ids_by_label)
              state.local_env
          in
          {
            state with
            local_env = Ir.LocalIdMap.add local_id value state.local_env;
          })
        state phi_instructions)
    states

and post_phi_block_flow (fixed_env : fixed_env) (block : Ir.block)
    (states : StateSet.t) : StateSet.t =
  let _, non_phi_instructions = split_block_phi_instructions block in
  let states =
    states |> StateSet.to_seq |> List.of_seq |> fun states ->
    List.fold_left
      (fun states (local_id, insn) ->
        let results, _ = interpret_instruction fixed_env states None insn in
        List.map
          (fun (state, value) ->
            {
              state with
              local_env = Ir.LocalIdMap.add local_id value state.local_env;
            })
          results)
      states non_phi_instructions
  in
  let states = StateSet.of_list states in
  states

and flow_branch (terminator : Ir.terminator) (flow_target : Ir.label)
    (states : StateSet.t) : StateSet.t =
  match terminator with
  | Ir.Br terminator_target when terminator_target = flow_target -> states
  | Ir.Cbr (local_id, true_label, false_label)
    when flow_target = true_label || flow_target = false_label ->
      StateSet.filter_map
        (fun state ->
          match Ir.LocalIdMap.find local_id state.local_env with
          | VBool v when v = (flow_target = true_label) -> Some state
          | VBool _ -> None
          | VUnknownBool -> Some state
          | _ -> failwith "Cbr called on non-boolean value")
        states
  | _ -> failwith "Unexpected flow"

and flow_return (terminator : Ir.terminator) (states : StateSet.t) :
    StateAndMaybeReturnSet.t =
  match terminator with
  | Ir.Ret (Some local_id) ->
      StateAndMaybeReturnSet.StateAndReturnSet
        (states |> StateSet.to_seq
        |> Seq.map (fun state ->
               (state, Ir.LocalIdMap.find local_id state.local_env))
        |> StateAndReturnSet.of_seq)
  | Ir.Ret None -> StateAndMaybeReturnSet.StateSet states
  | _ -> failwith "Unexpected flow"

and interpret_cfg_flow (fixed_env : fixed_env) (states : StateSet.t)
    (cfg : Ir.cfg) : StateAndMaybeReturnSet.t =
  let lift_no_return (f : StateSet.t -> StateSet.t) = function
    | StateAndMaybeReturnSet.StateSet states ->
        StateAndMaybeReturnSet.StateSet (f states)
    | StateAndMaybeReturnSet.StateAndReturnSet _ ->
        failwith "Return value in unexpected part of CFG"
  in
  let lift_return_out_only (f : StateSet.t -> StateAndMaybeReturnSet.t) =
    function
    | StateAndMaybeReturnSet.StateSet states -> f states
    | StateAndMaybeReturnSet.StateAndReturnSet _ ->
        failwith "Return value in unexpected part of CFG"
  in
  let module CfgFixpoint =
    Graph.Fixpoint.Make
      (Block_flow.G)
      (struct
        type vertex = Block_flow.G.E.vertex
        type edge = Block_flow.G.E.t
        type g = Block_flow.G.t
        type data = StateAndMaybeReturnSet.t option

        let direction = Graph.Fixpoint.Forward
        let equal = Flow.lift_equal StateAndMaybeReturnSet.equal
        let join = Flow.lift_join StateAndMaybeReturnSet.union

        let analyze =
          Block_flow.make_flow_function
            (fun source_block_name target_block ->
              lift_no_return @@ phi_block_flow source_block_name target_block)
            (fun block -> lift_no_return @@ post_phi_block_flow fixed_env block)
            (fun terminator flow_target ->
              lift_no_return @@ flow_branch terminator flow_target)
            (fun terminator -> lift_return_out_only @@ flow_return terminator)
            cfg
      end)
  in
  let g = Block_flow.flow_graph_of_cfg cfg in
  let init v =
    if Block_flow.G.in_degree g v = 0 then
      Some (StateAndMaybeReturnSet.StateSet states)
    else None
  in
  Option.get @@ CfgFixpoint.analyze init g Block_flow.Return

let init (fun_defs : Ir.fun_def list) (builtins : (string * builtin_fun) list) :
    fixed_env * state =
  let state =
    {
      heap = HeapIdMap.empty;
      local_env = Ir.LocalIdMap.empty;
      global_env = StringMap.empty;
      prints = [];
    }
  in
  let state =
    List.fold_left
      (fun state (name, _) ->
        let state, fun_heap_id = state_heap_add state (HBuiltinFun name) in
        let state, fun_ptr_heap_id =
          state_heap_add state (HValue (VPointer fun_heap_id))
        in
        if StringMap.mem name state.global_env then
          failwith "Duplicate builtin name";
        {
          state with
          global_env = StringMap.add name fun_ptr_heap_id state.global_env;
        })
      state builtins
  in
  let fixed_env = { fun_defs; builtin_funs = builtins } in
  (fixed_env, state)
