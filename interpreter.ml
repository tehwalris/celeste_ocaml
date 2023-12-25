let incr_mut r = r := !r + 1
let add_to_mut r v = r := !r + v

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

module ListForArrayTable = struct
  type item = heap_id
  type t = item Array.t

  let empty = [||]
  let get t i = t.(i)
  let append t item = Array.append t [| item |]
  let map = Array.map
  let length = Array.length
  let nth_opt t i = if i < Array.length t then Some t.(i) else None
end

type heap_value =
  | HValue of value
  | HObjectTable of (string * heap_id) list
  | HArrayTable of ListForArrayTable.t
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

let show_string_id_map show_v s =
  s |> StringMap.bindings
  |> List.map (fun (k, v) -> Printf.sprintf "\"%s\" -> %s" k (show_v v))
  |> String.concat "; "

type state = {
  heap : heap_value HeapIdMap.t;
  local_env : value Ir.LocalIdMap.t;
  outer_local_envs : value Ir.LocalIdMap.t list;
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

let map_value_references f v : value =
  match v with
  | VNumber _ -> v
  | VBool _ -> v
  | VUnknownBool -> v
  | VString _ -> v
  | VNil -> v
  | VPointer heap_id -> VPointer (f heap_id)

let map_heap_value_references f v : heap_value =
  match v with
  | HValue value -> HValue (map_value_references f value)
  | HObjectTable items -> HObjectTable (List.map (fun (k, v) -> (k, f v)) items)
  | HArrayTable items -> HArrayTable (ListForArrayTable.map f items)
  | HUnknownTable -> HUnknownTable
  | HClosure (global_id, captures) ->
      HClosure (global_id, List.map (map_value_references f) captures)
  | HBuiltinFun name -> HBuiltinFun name

let gc_heap (state : state) : state =
  Perf.count_and_time Perf.global_counters.gc @@ fun () ->
  let old_heap = state.heap in
  let new_heap = ref HeapIdMap.empty in
  let new_ids_by_old_ids = ref HeapIdMap.empty in
  let next_id = ref 0 in
  let rec visit old_id =
    match HeapIdMap.find_opt old_id !new_ids_by_old_ids with
    | Some new_id -> new_id
    | None ->
        let new_id = !next_id in
        next_id := !next_id + 1;
        new_ids_by_old_ids := HeapIdMap.add old_id new_id !new_ids_by_old_ids;
        assert (not (HeapIdMap.mem new_id !new_heap));
        let visited_value =
          map_heap_value_references visit (HeapIdMap.find old_id old_heap)
        in
        new_heap := HeapIdMap.add new_id visited_value !new_heap;
        new_id
  in

  let state =
    {
      heap = HeapIdMap.empty;
      global_env = StringMap.map visit state.global_env;
      local_env = Ir.LocalIdMap.map (map_value_references visit) state.local_env;
      outer_local_envs =
        List.map
          (Ir.LocalIdMap.map @@ map_value_references visit)
          state.outer_local_envs;
      prints = state.prints;
    }
  in
  { state with heap = !new_heap }

let normalize_state_maps_except_heap (state : state) : state =
  (* The = operator for maps considers the internal tree structure, not just the
     contained values like Map.equal. The result of this function is normalized so
     that = works correctly for our state by rebuilding all maps so that their
     internal tree structure is identical if their values are identical. *)
  Perf.count_and_time Perf.global_counters.normalize_state_maps_except_heap
  @@ fun () ->
  {
    heap = state.heap;
    global_env = state.global_env |> StringMap.to_seq |> StringMap.of_seq;
    local_env = state.local_env |> Ir.LocalIdMap.to_seq |> Ir.LocalIdMap.of_seq;
    outer_local_envs =
      List.map
        (fun local_env ->
          local_env |> Ir.LocalIdMap.to_seq |> Ir.LocalIdMap.of_seq)
        state.outer_local_envs;
    prints = state.prints;
  }

let normalize_state (state : state) : state =
  Printf.printf "normalize_state\n";
  state |> normalize_state_maps_except_heap |> gc_heap

module StateSet = Set.Make (struct
  type t = state

  let compare = Stdlib.compare
end)

module LazyStateSet = struct
  type t =
    | NormalizedSet of StateSet.t
    | NormalizedList of state list
    | NonNormalizedList of state list

  let empty = NormalizedList []

  let is_empty = function
    | NormalizedSet set -> StateSet.is_empty set
    | NormalizedList [] -> true
    | NormalizedList _ -> false
    | NonNormalizedList [] -> true
    | NonNormalizedList _ -> false

  let to_non_normalized_non_deduped_seq (t : t) : state Seq.t =
    match t with
    | NormalizedSet set -> StateSet.to_seq set
    | NormalizedList list -> List.to_seq list
    | NonNormalizedList list -> List.to_seq list

  let to_normalized_non_deduped_seq (t : t) : state Seq.t =
    match t with
    | NormalizedSet set -> StateSet.to_seq set
    | NormalizedList list -> List.to_seq list
    | NonNormalizedList list -> List.to_seq list |> Seq.map normalize_state

  let of_list (list : state list) : t = NonNormalizedList list

  let has_normalized_elements (t : t) : bool =
    match t with
    | NormalizedSet _ -> true
    | NormalizedList _ -> true
    | NonNormalizedList [] -> true
    | NonNormalizedList _ -> false

  let to_normalized_state_set (t : t) : StateSet.t =
    match t with
    | NormalizedSet t -> t
    | _ -> t |> to_normalized_non_deduped_seq |> StateSet.of_seq

  let normalize (t : t) : t = NormalizedSet (to_normalized_state_set t)

  let union (a : t) (b : t) : t =
    Printf.printf "union\n";
    if is_empty a then b
    else if is_empty b then a
    else
      let ab_list =
        Seq.append
          (to_non_normalized_non_deduped_seq a)
          (to_non_normalized_non_deduped_seq b)
        |> List.of_seq
      in
      if has_normalized_elements a && has_normalized_elements b then
        NormalizedList ab_list
      else NonNormalizedList ab_list

  let union_diff (a : t) (b : t) : t * t =
    Printf.printf "union diff b\n";
    let b = to_normalized_state_set b in
    Printf.printf "union diff rest\n";
    let union, diff =
      Seq.fold_left
        (fun (union, diff) v ->
          let new_union = StateSet.add v union in
          if new_union == union then (union, diff) else (new_union, v :: diff))
        (b, [])
        (to_normalized_non_deduped_seq a)
    in
    (NormalizedSet union, NormalizedList diff)

  let map (f : state -> state) (t : t) : t =
    let changed = ref false in
    let new_list =
      t |> to_non_normalized_non_deduped_seq
      |> Seq.map (fun state ->
             let new_state = f state in
             if new_state != state then changed := true;
             new_state)
      |> List.of_seq
    in
    if !changed then NonNormalizedList new_list else t

  let filter (f : state -> bool) = function
    | NormalizedSet set -> NormalizedSet (StateSet.filter f set)
    | NormalizedList list -> NormalizedList (List.filter f list)
    | NonNormalizedList list -> NonNormalizedList (List.filter f list)

  let cardinal_upper_bound (t : t) : int =
    match t with
    | NormalizedSet set -> StateSet.cardinal set
    | NormalizedList list -> List.length list
    | NonNormalizedList list -> List.length list
end

module StateAndReturnSet = Set.Make (struct
  type t = state * value

  let compare = Stdlib.compare
end)

module StateAndMaybeReturnSet = struct
  type t =
    | StateSet of LazyStateSet.t
    | StateAndReturnSet of StateAndReturnSet.t

  let union (a : t) (b : t) : t =
    match (a, b) with
    | StateSet a, StateSet b -> StateSet (LazyStateSet.union a b)
    | StateAndReturnSet a, StateAndReturnSet b ->
        StateAndReturnSet (StateAndReturnSet.union a b)
    | _ -> failwith "Cannot union StateSet and StateAndReturnSet"

  let state_and_return_set_union_diff (a : StateAndReturnSet.t)
      (b : StateAndReturnSet.t) : StateAndReturnSet.t * StateAndReturnSet.t =
    StateAndReturnSet.fold
      (fun v (union, diff) ->
        let new_union = StateAndReturnSet.add v union in
        if new_union == union then (union, diff)
        else (new_union, StateAndReturnSet.add v diff))
      a
      (b, StateAndReturnSet.empty)

  (* union_diff a b = (union a b, diff a b) (only when = is Set.equal though)*)
  let union_diff (a : t) (b : t) : t * t =
    match (a, b) with
    | StateSet a, StateSet b ->
        let union, diff = LazyStateSet.union_diff a b in
        (StateSet union, StateSet diff)
    | StateAndReturnSet a, StateAndReturnSet b ->
        let union, diff = state_and_return_set_union_diff a b in
        (StateAndReturnSet union, StateAndReturnSet diff)
    | _ -> failwith "Cannot union_diff StateSet and StateAndReturnSet"

  let is_empty (a : t) : bool =
    match a with
    | StateSet a -> LazyStateSet.is_empty a
    | StateAndReturnSet a -> StateAndReturnSet.is_empty a
end

let analyze_live_variables cfg =
  let module LiveVariableAnalysis =
    Graph.Fixpoint.Make
      (Flow.G)
      (struct
        type vertex = Flow.G.E.vertex
        type edge = Flow.G.E.t
        type g = Flow.G.t
        type data = Ir.LocalIdSet.t option

        let direction = Graph.Fixpoint.Backward
        let equal = Flow.lift_equal Ir.LocalIdSet.equal
        let join = Flow.lift_join Ir.LocalIdSet.union

        let analyze =
          Flow.make_flow_function Liveness.flow_instruction_live_variables
            Liveness.flow_terminator_live_variables cfg
      end)
  in
  let g = Flow.flow_graph_of_cfg cfg in
  LiveVariableAnalysis.analyze (fun _ -> Some Ir.LocalIdSet.empty) g

type terminator_result =
  (* each item corresponds to an input state *)
  | Ret of value list option
  (* each item might _not_ correspond to an input state *)
  | Br of (Ir.label * state) list

let interpret_unary_op (state : state) (op : string) (v : value) : value =
  match (op, v) with
  | "-", VNumber v -> VNumber (Pico_number.neg v)
  | "not", VBool v -> VBool (not v)
  | "not", VUnknownBool -> VUnknownBool
  | "#", VString v -> VNumber (Pico_number.of_int @@ String.length v)
  | "#", VPointer heap_id -> (
      let table = HeapIdMap.find heap_id state.heap in
      match table with
      | HArrayTable items ->
          VNumber (Pico_number.of_int @@ ListForArrayTable.length items)
      | HUnknownTable -> VNumber (Pico_number.of_int 0)
      | _ -> failwith @@ Printf.sprintf "Expected HArrayTable or HUnknownTable")
  | op, v ->
      failwith @@ Printf.sprintf "Unsupported unary op: %s %s" op (show_value v)

let interpret_binary_op (l : value) (op : string) (r : value) : value =
  let is_simple_value v =
    match v with
    | VNumber _ -> true
    | VBool _ -> true
    | VUnknownBool -> false
    | VString _ -> true
    | VNil -> true
    | VPointer _ -> false
  in
  match (l, op, r) with
  | a, "==", b when is_simple_value a && is_simple_value b -> VBool (a = b)
  | a, "~=", b when is_simple_value a && is_simple_value b -> VBool (a <> b)
  | VNumber l, "+", VNumber r -> VNumber (Pico_number.add l r)
  | VNumber l, "-", VNumber r -> VNumber (Pico_number.sub l r)
  | VNumber l, "*", VNumber r -> VNumber (Pico_number.mul l r)
  | VNumber l, "/", VNumber r -> VNumber (Pico_number.div l r)
  | VNumber l, "%", VNumber r -> VNumber (Pico_number.modulo l r)
  | VNumber l, "<", VNumber r -> VBool (Int32.compare l r < 0)
  | VNumber l, "<=", VNumber r -> VBool (Int32.compare l r <= 0)
  | VNumber l, ">", VNumber r -> VBool (Int32.compare l r > 0)
  | VNumber l, ">=", VNumber r -> VBool (Int32.compare l r >= 0)
  | VString l, "..", VString r -> VString (l ^ r)
  | VString l, "..", VNumber r ->
      VString (l ^ Int.to_string @@ Pico_number.int_of r)
  | VNumber l, "..", VString r ->
      VString ((Int.to_string @@ Pico_number.int_of l) ^ r)
  | l, op, r ->
      failwith
      @@ Printf.sprintf "Unsupported binary op: %s %s %s" (show_value l) op
           (show_value r)

let debug_states (states : LazyStateSet.t) : string =
  if LazyStateSet.has_normalized_elements states then "normalized"
  else "not normalized"

let rec interpret_non_phi_instruction (fixed_env : fixed_env)
    (states : state list) (insn : Ir.instruction) :
    (state * value) list * Ir.label option =
  incr_mut Perf.global_counters.interpret_non_phi_instruction;
  if
    !(Perf.global_counters.enable_printing)
    && !(Perf.global_counters.interpret_non_phi_instruction) mod 1000 = 0
  then Perf.print_counters ();
  let handle_separately_no_phi (f : state -> state * value) :
      (state * value) list * Ir.label option =
    Perf.count_and_time Perf.global_counters.handle_separately_no_phi
    @@ fun () -> (List.map f states, None)
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
            state_heap_update state (fun _ -> HUnknownTable) heap_id
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
            | HUnknownTable -> ListForArrayTable.empty
            | _ ->
                failwith
                  "GetIndex called on something that's not an array-like table \
                   or unknown table"
          in
          let state, field_heap_id =
            match ListForArrayTable.nth_opt old_fields (index - 1) with
            | Some field_heap_id -> (state, field_heap_id)
            | None ->
                if not create_if_missing then
                  failwith "Index not found, but create_if_missing was false";
                let state, field_heap_id = state_heap_add state (HValue VNil) in
                let state =
                  state_heap_update state
                    (fun _ ->
                      HArrayTable
                        (ListForArrayTable.append old_fields field_heap_id))
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
                incr_mut Perf.global_counters.builtin_call;
                let builtin_fun = List.assoc name fixed_env.builtin_funs in
                [ builtin_fun state arg_values ]
            | HClosure (fun_global_id, captured_values) ->
                incr_mut Perf.global_counters.closure_call;
                let fun_def =
                  List.find
                    (fun def -> def.Ir.name = fun_global_id)
                    fixed_env.fun_defs
                in
                let padded_arg_values =
                  if List.length arg_values < List.length fun_def.Ir.arg_ids
                  then
                    arg_values
                    @ List.init
                        (List.length fun_def.Ir.arg_ids - List.length arg_values)
                        (fun _ -> VNil)
                  else if
                    List.length arg_values > List.length fun_def.Ir.arg_ids
                  then BatList.take (List.length fun_def.Ir.arg_ids) arg_values
                  else arg_values
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
                        (captured_values @ padded_arg_values);
                    outer_local_envs = state.local_env :: state.outer_local_envs;
                  }
                in
                interpret_cfg fixed_env
                  (LazyStateSet.of_list [ inner_state ])
                  fun_def.cfg
                |> (function
                     | StateAndMaybeReturnSet.StateSet inner_states ->
                         inner_states
                         |> LazyStateSet.to_non_normalized_non_deduped_seq
                         |> Seq.map (fun inner_state -> (inner_state, VNil))
                     | StateAndMaybeReturnSet.StateAndReturnSet
                         inner_states_and_returns ->
                         inner_states_and_returns |> StateAndReturnSet.to_seq)
                |> Seq.map (fun (inner_state, return_value) ->
                       let state =
                         {
                           heap = inner_state.heap;
                           local_env = List.hd inner_state.outer_local_envs;
                           outer_local_envs =
                             List.tl inner_state.outer_local_envs;
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
          (state, interpret_unary_op state op value))
  | BinaryOp (left_local_id, op, right_local_id) ->
      handle_separately_no_phi (fun state ->
          let left_value = Ir.LocalIdMap.find left_local_id state.local_env in
          let right_value = Ir.LocalIdMap.find right_local_id state.local_env in
          (state, interpret_binary_op left_value op right_value))
  | Phi _ -> failwith "Phi nodes should be handled by phi_block_flow"

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

and flow_block_phi (source_block_name : Ir.label) (target_block : Ir.block)
    (states : LazyStateSet.t) : LazyStateSet.t =
  Printf.printf "before flow_block_phi (%s)\n" @@ debug_states states;
  let phi_instructions, _ = Ir.split_block_phi_instructions target_block in
  LazyStateSet.map
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

and flow_block_before_join
    (live_variable_analysis :
      Flow.flow_side * Ir.local_id -> Ir.LocalIdSet.t option)
    (target_block : Ir.block) (states : LazyStateSet.t) : LazyStateSet.t =
  Printf.printf "before flow_block_before_join (%s)\n" @@ debug_states states;
  let terminator_local_id, _ = target_block.terminator in
  let first_non_phi_local_id =
    target_block.instructions
    |> List.find_map (function
         | _, Ir.Phi _ -> None
         | local_id, _ -> Some local_id)
    |> Option.value ~default:terminator_local_id
  in
  let live_variables =
    Option.get @@ live_variable_analysis (Flow.Before, first_non_phi_local_id)
  in
  let states =
    LazyStateSet.map
      (fun state ->
        let new_local_env =
          Ir.LocalIdMap.filter
            (fun local_id _ -> Ir.LocalIdSet.mem local_id live_variables)
            state.local_env
        in
        if new_local_env == state.local_env then state
        else { state with local_env = new_local_env })
      states
  in
  Printf.printf "after flow_block_before_join (%s)\n" @@ debug_states states;
  states

and flow_block_post_phi (fixed_env : fixed_env) (block : Ir.block)
    (states : LazyStateSet.t) : LazyStateSet.t =
  let _, non_phi_instructions = Ir.split_block_phi_instructions block in
  let states =
    states |> LazyStateSet.to_non_normalized_non_deduped_seq |> List.of_seq
  in
  let states =
    List.fold_left
      (fun states (local_id, insn) ->
        let results, _ = interpret_non_phi_instruction fixed_env states insn in
        List.map
          (fun (state, value) ->
            {
              state with
              local_env = Ir.LocalIdMap.add local_id value state.local_env;
            })
          results)
      states non_phi_instructions
  in
  let states = LazyStateSet.of_list states in
  states

and flow_branch (terminator : Ir.terminator) (flow_target : Ir.label)
    (states : LazyStateSet.t) : LazyStateSet.t =
  Printf.printf "before flow_branch (%s)\n" @@ debug_states states;
  match terminator with
  | Ir.Br terminator_target when terminator_target = flow_target -> states
  | Ir.Cbr (local_id, true_label, false_label)
    when flow_target = true_label || flow_target = false_label ->
      LazyStateSet.filter
        (fun state ->
          match Ir.LocalIdMap.find local_id state.local_env with
          | VBool v when v = (flow_target = true_label) -> true
          | VBool _ -> false
          | VUnknownBool -> true
          | _ -> failwith "Cbr called on non-boolean value")
        states
  | _ -> failwith "Unexpected flow"

and flow_return (terminator : Ir.terminator) (states : LazyStateSet.t) :
    StateAndMaybeReturnSet.t =
  let clean_state_after_return preserved_local_id state =
    {
      state with
      local_env =
        (match preserved_local_id with
        | Some local_id ->
            Ir.LocalIdMap.singleton local_id
            @@ Ir.LocalIdMap.find local_id state.local_env
        | None -> Ir.LocalIdMap.empty);
    }
  in
  match terminator with
  | Ir.Ret (Some local_id) ->
      StateAndMaybeReturnSet.StateAndReturnSet
        (states |> LazyStateSet.to_non_normalized_non_deduped_seq
        |> Seq.map (fun state ->
               let state = clean_state_after_return (Some local_id) state in
               (state, Ir.LocalIdMap.find local_id state.local_env))
        |> StateAndReturnSet.of_seq)
  | Ir.Ret None ->
      StateAndMaybeReturnSet.StateSet
        (LazyStateSet.map (clean_state_after_return None) states)
  | _ -> failwith "Unexpected flow"

and interpret_cfg (fixed_env : fixed_env) (states : LazyStateSet.t)
    (cfg : Ir.cfg) : StateAndMaybeReturnSet.t =
  let lift_no_return (f : LazyStateSet.t -> LazyStateSet.t) = function
    | StateAndMaybeReturnSet.StateSet states ->
        StateAndMaybeReturnSet.StateSet (f states)
    | StateAndMaybeReturnSet.StateAndReturnSet _ ->
        failwith "Return value in unexpected part of CFG"
  in
  let lift_return_out_only (f : LazyStateSet.t -> StateAndMaybeReturnSet.t) =
    function
    | StateAndMaybeReturnSet.StateSet states -> f states
    | StateAndMaybeReturnSet.StateAndReturnSet _ ->
        failwith "Return value in unexpected part of CFG"
  in
  let live_variable_analysis = analyze_live_variables cfg in
  let module CfgFixpoint =
    Custom_fixpoint.Make
      (Block_flow.G)
      (struct
        type vertex = Block_flow.G.E.vertex
        type edge = Block_flow.G.E.t
        type g = Block_flow.G.t
        type data = StateAndMaybeReturnSet.t option

        let empty = None
        let untimed_join = Flow.lift_join StateAndMaybeReturnSet.union

        let join a b =
          Perf.count_and_time Perf.global_counters.flow_join @@ fun () ->
          untimed_join a b

        let accumulate (accumulated : StateAndMaybeReturnSet.t option)
            (potentially_new : StateAndMaybeReturnSet.t option) :
            StateAndMaybeReturnSet.t option
            * StateAndMaybeReturnSet.t option option =
          Perf.count_and_time Perf.global_counters.flow_accumulate @@ fun () ->
          let is_empty = function
            | Some states -> StateAndMaybeReturnSet.is_empty states
            | None -> true
          in
          if is_empty potentially_new then (accumulated, None)
          else if is_empty accumulated then
            (potentially_new, Some potentially_new)
          else
            let join_result, actually_new =
              StateAndMaybeReturnSet.union_diff
                (Option.get potentially_new)
                (Option.get accumulated)
            in
            ( Some join_result,
              if StateAndMaybeReturnSet.is_empty actually_new then None
              else Some (Some actually_new) )

        let analyze =
          let inner =
            Block_flow.make_flow_function
              (fun source_block_name target_block ->
                lift_no_return @@ flow_block_phi source_block_name target_block)
              (fun target_block ->
                lift_no_return
                @@ flow_block_before_join live_variable_analysis target_block)
              (fun block ->
                lift_no_return @@ flow_block_post_phi fixed_env block)
              (fun terminator flow_target ->
                lift_no_return @@ flow_branch terminator flow_target)
              (fun terminator -> lift_return_out_only @@ flow_return terminator)
              cfg
          in
          fun edge data ->
            Perf.count_and_time Perf.global_counters.flow_analyze @@ fun () ->
            inner edge data

        let show_data = function
          | Some (StateAndMaybeReturnSet.StateSet states) ->
              Printf.sprintf "Some(StateSet %d)"
              @@ LazyStateSet.cardinal_upper_bound states
          | Some (StateAndMaybeReturnSet.StateAndReturnSet states) ->
              Printf.sprintf "Some(StateAndReturnSet %d)"
              @@ StateAndReturnSet.cardinal states
          | None -> "None"

        let show_vertex = Block_flow.show_flow_node
      end)
  in
  let g = Block_flow.flow_graph_of_cfg cfg in
  add_to_mut Perf.global_counters.fixpoint_created_node
  @@ Block_flow.G.nb_vertex g;
  add_to_mut Perf.global_counters.fixpoint_created_edge
  @@ Block_flow.G.nb_edges g;
  let init v =
    if Block_flow.G.in_degree g v = 0 then
      Some (StateAndMaybeReturnSet.StateSet states)
    else None
  in
  Perf.count_and_time Perf.global_counters.fixpoint @@ fun () ->
  Option.get @@ CfgFixpoint.analyze true init g Block_flow.Return

let init (fun_defs : Ir.fun_def list) (builtins : (string * builtin_fun) list) :
    fixed_env * state =
  let state =
    {
      heap = HeapIdMap.empty;
      local_env = Ir.LocalIdMap.empty;
      outer_local_envs = [];
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
