type heap_id = int [@@deriving show]

let gen_heap_id : unit -> heap_id =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

type value =
  | VNumber of Pico_number.t
  | VBool of bool
  | VString of string
  | VNil
  | VPointer of heap_id

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
}

type builtin_fun = state -> value list -> state

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

let interpret_instruction (fixed_env : fixed_env) (state : state)
    (insn : Ir.instruction) : state * value =
  match insn with
  | Alloc ->
      let heap_id = gen_heap_id () in
      ( { state with heap = HeapIdMap.add heap_id (HValue VNil) state.heap },
        VPointer heap_id )
  | GetGlobal (name, create_if_missing) ->
      let heap_id, state =
        match StringMap.find_opt name state.global_env with
        | Some heap_id -> (heap_id, state)
        | None ->
            if not create_if_missing then
              failwith "Global not found, but create_if_missing was false";
            let state, heap_id = state_heap_add state (HValue VNil) in
            let state =
              {
                state with
                global_env = StringMap.add name heap_id state.global_env;
              }
            in
            (heap_id, state)
      in
      (state, VPointer heap_id)
  | Load local_id ->
      let heap_id = heap_id_from_pointer_local state local_id in
      let value =
        match HeapIdMap.find heap_id state.heap with
        | HValue value -> value
        | _ -> failwith "Value is of a type that can not be stored in a local"
      in
      (state, value)
  | Store (target_local_id, source_local_id) ->
      let heap_id = heap_id_from_pointer_local state target_local_id in
      let source_value = Ir.LocalIdMap.find source_local_id state.local_env in
      let state =
        state_heap_update state (fun _ -> HValue source_value) heap_id
      in
      (state, VNil)
  | StoreEmptyTable local_id ->
      let heap_id = heap_id_from_pointer_local state local_id in
      let state = state_heap_update state (fun _ -> HObjectTable []) heap_id in
      (state, VNil)
  | StoreClosure (target_local_id, fun_global_id, captured_local_ids) ->
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
      (state, VNil)
  | GetField (table_local_id, field_name, create_if_missing) ->
      let table_heap_id = heap_id_from_pointer_local state table_local_id in
      let old_fields =
        match HeapIdMap.find table_heap_id state.heap with
        | HObjectTable old_fields -> old_fields
        | HUnknownTable -> []
        | _ ->
            failwith
              "GetField called on something that's not an object-like table or \
               unknown table"
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
      (state, VPointer field_heap_id)
  | GetIndex (table_local_id, index_local_id, create_if_missing) ->
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
              "GetIndex called on something that's not an array-like table or \
               unknown table"
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
      (state, VPointer field_heap_id)
  | NumberConstant v -> (state, VNumber v)
  | BoolConstant v -> (state, VBool v)
  | StringConstant v -> (state, VString v)
  | NilConstant -> (state, VNil)
  | Call (fun_local_id, arg_local_ids) -> (
      let arg_values =
        List.map (fun id -> Ir.LocalIdMap.find id state.local_env) arg_local_ids
      in
      let fun_heap_id = heap_id_from_pointer_local state fun_local_id in
      match HeapIdMap.find fun_heap_id state.heap with
      | HBuiltinFun name ->
          let builtin_fun = List.assoc name fixed_env.builtin_funs in
          let state = builtin_fun state arg_values in
          (state, VNil)
      | HClosure _ -> failwith "TODO closure call not implemented"
      | _ -> failwith "Calling something that's not a function")
  | UnaryOp _ -> failwith "TODO UnaryOp instruction not implemented"
  | BinaryOp _ -> failwith "TODO BinaryOp instruction not implemented"
  | Phi _ -> failwith "TODO Phi instruction not implemented"

type terminator_result = Ret of value option | Br of Ir.label

let interpret_terminator (state : state) (terminator : Ir.terminator) :
    terminator_result =
  match terminator with
  | Ir.Ret (Some local_id) ->
      let value = Ir.LocalIdMap.find local_id state.local_env in
      Ret (Some value)
  | Ir.Ret None -> Ret None
  | Ir.Br label -> Br label
  | Ir.Cbr (local_id, true_label, false_label) -> (
      match Ir.LocalIdMap.find local_id state.local_env with
      | VBool true -> Br true_label
      | VBool false -> Br false_label
      | _ -> failwith "Cbr called on non-boolean value")

let interpret_block (fixed_env : fixed_env) (state : state) (block : Ir.block) :
    state * terminator_result =
  let state =
    List.fold_left
      (fun state (local_id, insn) ->
        let state, value = interpret_instruction fixed_env state insn in
        {
          state with
          local_env = Ir.LocalIdMap.add local_id value state.local_env;
        })
      state block.instructions
  in
  let _, terminator = block.terminator in
  let terminator_result = interpret_terminator state terminator in
  (state, terminator_result)

let interpret_cfg (fixed_env : fixed_env) (state : state) (cfg : Ir.cfg) :
    value option =
  let rec interpret_block_rec (state : state) (block : Ir.block) : value option
      =
    let state, terminator_result = interpret_block fixed_env state block in
    match terminator_result with
    | Ret value -> value
    | Br label -> interpret_block_rec state @@ List.assoc label cfg.named
  in
  interpret_block_rec state cfg.entry

let init (fun_defs : Ir.fun_def list) (builtins : (string * builtin_fun) list) :
    fixed_env * state =
  let state =
    {
      heap = HeapIdMap.empty;
      local_env = Ir.LocalIdMap.empty;
      global_env = StringMap.empty;
    }
  in
  let state =
    List.fold_left
      (fun state (name, _) ->
        let state, heap_id = state_heap_add state (HBuiltinFun name) in
        if StringMap.mem name state.global_env then
          failwith "Duplicate builtin name";
        { state with global_env = StringMap.add name heap_id state.global_env })
      state builtins
  in
  let fixed_env = { fun_defs; builtin_funs = builtins } in
  (fixed_env, state)
