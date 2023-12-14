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

type terminator_result =
  (* each item corresponds to an input state *)
  | Ret of value list option
  (* each item might _not_ correspond to an input state *)
  | Br of (Ir.label * state) list

let interpret_unary_op (op : string) (v : value) : value =
  match (op, v) with
  | "-", VNumber v -> VNumber (Pico_number.neg v)
  | "not", VBool v -> VBool (not v)
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
      (* TODO Process the whole (state list) at once when calling, instead of handling each state separately. *)
      handle_separately_no_phi (fun state ->
          let arg_values =
            List.map
              (fun id -> Ir.LocalIdMap.find id state.local_env)
              arg_local_ids
          in
          let fun_heap_id = heap_id_from_pointer_local state fun_local_id in
          match HeapIdMap.find fun_heap_id state.heap with
          | HBuiltinFun name ->
              let builtin_fun = List.assoc name fixed_env.builtin_funs in
              let state = builtin_fun state arg_values in
              (state, VNil)
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
                      (fun locals id value -> Ir.LocalIdMap.add id value locals)
                      Ir.LocalIdMap.empty
                      (fun_def.Ir.capture_ids @ fun_def.Ir.arg_ids)
                      (captured_values @ arg_values);
                }
              in
              let inner_state, return_value =
                interpret_cfg_single_state fixed_env inner_state fun_def.cfg
              in
              let state =
                {
                  heap = inner_state.heap;
                  local_env = state.local_env;
                  global_env = inner_state.global_env;
                  prints = inner_state.prints;
                }
              in
              (state, Option.value return_value ~default:VNil)
          | _ -> failwith "Calling something that's not a function")
  | UnaryOp (op, local_id) ->
      handle_separately_no_phi (fun state ->
          let value = Ir.LocalIdMap.find local_id state.local_env in
          (state, interpret_unary_op op value))
  | BinaryOp (left_local_id, op, right_local_id) ->
      handle_separately_no_phi (fun state ->
          let left_value = Ir.LocalIdMap.find left_local_id state.local_env in
          let right_value = Ir.LocalIdMap.find right_local_id state.local_env in
          (state, interpret_binary_op left_value op right_value))
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
        (List.map
           (fun state ->
             let label =
               match Ir.LocalIdMap.find local_id state.local_env with
               | VBool true -> true_label
               | VBool false -> false_label
               | _ -> failwith "Cbr called on non-boolean value"
             in
             (label, state))
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
