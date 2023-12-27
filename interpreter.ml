let debug_prints = ref false
let incr_mut r = r := !r + 1
let add_to_mut r v = r := !r + v

type heap_id = int [@@deriving show]

let gen_heap_id : unit -> heap_id =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

type scalar_value =
  | SNumber of Pico_number.t
  | SBool of bool
  | SUnknownBool
  | SString of string
  | SNil of string option
  | SPointer of heap_id
  | SNilPointer of string
[@@deriving show]

type vector_value = VNumber of Pico_number.t Array.t

let show_vector_value = function
  | VNumber a -> Printf.sprintf "VNumber[%d]" @@ Array.length a

let pp_vector_value fmt v = Format.pp_print_string fmt @@ show_vector_value v

type value = Scalar of scalar_value | Vector of vector_value [@@deriving show]

let length_of_vector = function VNumber a -> Array.length a

let seq_of_vector = function
  | VNumber a -> a |> Array.to_seq |> Seq.map (fun v -> SNumber v)

let seq_of_value = function
  | Scalar s -> List.to_seq [ s ]
  | Vector v -> seq_of_vector v

let example_of_vector vec =
  vec |> seq_of_vector |> Seq.uncons |> Option.get |> fst

let can_vectorize_scalar_value = function SNumber _ -> true | _ -> false

let can_vectorize_value = function
  | Scalar s -> can_vectorize_scalar_value s
  | Vector vec -> can_vectorize_scalar_value @@ example_of_vector vec

let vector_of_seq_example seq scalar_example =
  let fail_mixed () = failwith "Cannot build vector of mixed types" in
  match scalar_example with
  | SNumber _ ->
      VNumber
        (seq
        |> Seq.map (function SNumber n -> n | _ -> fail_mixed ())
        |> Array.of_seq)
  | _ ->
      assert (not @@ can_vectorize_scalar_value scalar_example);
      failwith
      @@ Printf.sprintf "Cannot build vector from %s"
      @@ show_scalar_value scalar_example

let vector_of_non_empty_seq seq =
  let scalar_example, _ = Option.get @@ Seq.uncons seq in
  vector_of_seq_example seq scalar_example

let map_vector (f : scalar_value -> scalar_value) vec =
  let length = length_of_vector vec in
  if length = 0 then vec
  else seq_of_vector vec |> Seq.map f |> vector_of_non_empty_seq

let map2_vector (f : scalar_value -> scalar_value -> scalar_value) vec1 vec2 =
  let length1 = length_of_vector vec1 in
  let length2 = length_of_vector vec2 in
  assert (length1 = length2);
  if length1 = 0 || length2 = 0 then vec1
  else
    let mapped_seq =
      seq_of_vector vec1
      |> Seq.zip (seq_of_vector vec2)
      |> Seq.map (fun (v1, v2) -> f v1 v2)
    in
    let scalar_example, _ = Option.get @@ Seq.uncons mapped_seq in
    vector_of_seq_example mapped_seq scalar_example

module ListForArrayTable = struct
  type t = heap_id Array.t

  let empty = [||]
  let is_empty t = Array.length t = 0
  let get t i = t.(i)
  let append t item = Array.append t [| item |]
  let map = Array.map
  let length = Array.length
  let nth_opt t i = if i < Array.length t then Some t.(i) else None
  let to_seq = Array.to_seq
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

module Heap = struct
  type t = {
    old_values : heap_value Array.t;
    old_changed : bool Array.t; (* mutated, but only from false to true *)
    new_values : heap_value HeapIdMap.t;
  }

  type storage_location = Old | New | NewOrOld

  let find_storage_location (heap_id : heap_id) (heap : t) =
    if heap_id >= Array.length heap.old_values then New
    else if heap.old_changed.(heap_id) then NewOrOld
    else Old

  let empty =
    { old_values = [||]; old_changed = [||]; new_values = HeapIdMap.empty }

  let find (heap_id : heap_id) (heap : t) : heap_value =
    let find_old () = heap.old_values.(heap_id) in
    let find_new_opt () = HeapIdMap.find_opt heap_id heap.new_values in
    match find_storage_location heap_id heap with
    | Old -> find_old ()
    | New -> Option.get @@ find_new_opt ()
    | NewOrOld -> (
        match find_new_opt () with Some v -> v | None -> find_old ())

  let add (heap_id : heap_id) (value : heap_value) (heap : t) : t =
    if find_storage_location heap_id heap <> New then
      heap.old_changed.(heap_id) <- true;
    { heap with new_values = HeapIdMap.add heap_id value heap.new_values }

  let old_of_list (s : (heap_id * heap_value) List.t) : t =
    List.iteri (fun i (heap_id, _) -> assert (heap_id = i)) s;
    {
      old_values = s |> List.to_seq |> Seq.map snd |> Array.of_seq;
      old_changed = Array.make (List.length s) false;
      new_values = HeapIdMap.empty;
    }

  let old_of_seq (s : (heap_id * heap_value) Seq.t) : t =
    s |> List.of_seq |> old_of_list

  let seq_of_old (heap : t) : (heap_id * heap_value) Seq.t =
    assert (HeapIdMap.is_empty heap.new_values);
    Array.to_seqi heap.old_values

  let map (f : heap_value -> heap_value) (heap : t) : t =
    {
      old_values = heap.old_values |> Array.map f;
      old_changed = heap.old_changed;
      new_values = heap.new_values |> HeapIdMap.map f;
    }
end

module StringMap = Map.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

let show_string_id_map show_v s =
  s |> StringMap.bindings
  |> List.map (fun (k, v) -> Printf.sprintf "\"%s\" -> %s" k (show_v v))
  |> String.concat "; "

type state = {
  heap : Heap.t;
  local_env : value Ir.LocalIdMap.t;
  outer_local_envs : value Ir.LocalIdMap.t list;
  global_env : heap_id StringMap.t;
  prints : string list;
  vector_size : int;
}

type builtin_fun = state -> value list -> state * value

let failwith_not_pointer (v : value) =
  match v with
  | Scalar (SPointer _) | Scalar (SNilPointer _) ->
      failwith "failwith_not_pointer called with pointer"
  | Scalar (SNil (Some hint)) ->
      failwith
      @@ Printf.sprintf "Value is not a pointer (value is nil; %s)" hint
  | Scalar (SNil None) ->
      failwith @@ Printf.sprintf "Value is not a pointer (value is nil)"
  | _ -> failwith "Value is not a pointer"

let heap_id_from_pointer_local (state : state) (local_id : Ir.local_id) :
    heap_id =
  match Ir.LocalIdMap.find local_id state.local_env with
  | Scalar (SPointer heap_id) -> heap_id
  | Scalar (SNilPointer hint) ->
      failwith @@ Printf.sprintf "Attempted to dereference nil (%s)" hint
  | v -> failwith_not_pointer v

let state_heap_add (state : state) (heap_value : heap_value) : state * heap_id =
  let heap_id = gen_heap_id () in
  let state = { state with heap = Heap.add heap_id heap_value state.heap } in
  (state, heap_id)

let state_heap_update (state : state) (update : heap_value -> heap_value)
    (heap_id : heap_id) : state =
  let old_heap_value = Heap.find heap_id state.heap in
  let new_heap_value = update old_heap_value in
  { state with heap = Heap.add heap_id new_heap_value state.heap }

let map_value_references f v : value =
  match v with
  | Scalar (SNumber _) -> v
  | Scalar (SBool _) -> v
  | Scalar SUnknownBool -> v
  | Scalar (SString _) -> v
  | Scalar (SNil _) -> v
  | Scalar (SPointer heap_id) -> Scalar (SPointer (f heap_id))
  | Scalar (SNilPointer _) -> v
  | Vector v -> Vector v

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
  let new_heap_values = ref [] in
  let new_ids_by_old_ids = ref HeapIdMap.empty in
  let next_id = ref 0 in
  let rec visit old_id =
    match HeapIdMap.find_opt old_id !new_ids_by_old_ids with
    | Some new_id -> new_id
    | None ->
        let new_id = !next_id in
        next_id := !next_id + 1;
        new_ids_by_old_ids := HeapIdMap.add old_id new_id !new_ids_by_old_ids;
        (* ref is so that we can add to the list before recursing *)
        let visited_value_ref = ref None in
        new_heap_values := (new_id, visited_value_ref) :: !new_heap_values;
        visited_value_ref :=
          Some (map_heap_value_references visit (Heap.find old_id old_heap));
        new_id
  in

  let state =
    {
      heap = Heap.empty;
      global_env = StringMap.map visit state.global_env;
      local_env = Ir.LocalIdMap.map (map_value_references visit) state.local_env;
      outer_local_envs =
        List.map
          (Ir.LocalIdMap.map @@ map_value_references visit)
          state.outer_local_envs;
      prints = state.prints;
      vector_size = state.vector_size;
    }
  in

  {
    state with
    heap =
      !new_heap_values
      |> List.rev_map (fun (id, v) -> (id, Option.get !v))
      |> Heap.old_of_list;
  }

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
    vector_size = state.vector_size;
  }

let normalize_state (state : state) : state =
  if !debug_prints then Printf.printf "normalize_state\n";
  state |> normalize_state_maps_except_heap |> gc_heap

module StateSet = Set.Make (struct
  type t = state

  let compare = Stdlib.compare
end)

module StateMap = Map.Make (struct
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
    if !debug_prints then Printf.printf "union\n";
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
    if !debug_prints then Printf.printf "union diff b\n";
    let b = to_normalized_state_set b in
    if !debug_prints then Printf.printf "union diff rest\n";
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

let map_state_values (f : value -> value) (state : state) : state =
  let f_heap_value = function HValue v -> HValue (f v) | v -> v in
  {
    state with
    heap = Heap.map f_heap_value state.heap (* TODO map other fields *);
  }

let zip_seq_list (seq_list : 'a Seq.t list) : 'a list Seq.t =
  let rec zip_seq_list_helper (seq_list : 'a Seq.t list) : 'a list Seq.t =
    match seq_list with
    | [] -> Seq.empty
    | first_seq :: _ -> (
        match Seq.uncons first_seq with
        | None ->
            assert (List.for_all (fun seq -> Seq.is_empty seq) seq_list);
            Seq.empty
        | Some _ ->
            let seq_heads, seq_tails =
              seq_list
              |> List.map (fun v -> Option.get @@ Seq.uncons v)
              |> List.split
            in
            fun () -> Seq.Cons (seq_heads, zip_seq_list_helper seq_tails))
  in

  zip_seq_list_helper seq_list

let zip_map_map (f : 'v list -> 'v) (to_seq : 'm -> ('k * 'v) Seq.t)
    (of_seq : ('k * 'v) Seq.t -> 'm) (maps : 'm list) : 'm =
  maps |> List.rev_map to_seq |> zip_seq_list
  |> Seq.map (fun kv_list : ('k * 'v) ->
         let k, v_list =
           Option.get
           @@ List.fold_right
                (fun (k, v) acc ->
                  match acc with
                  | Some (only_key, values) ->
                      assert (k = only_key);
                      Some (k, v :: values)
                  | None -> Some (k, [ v ]))
                kv_list None
         in
         (k, f v_list))
  |> of_seq

let zip_map_state_values (f : value list -> value) (shape : state)
    (states : state list) : state =
  let f_heap_value (heap_values : heap_value list) : heap_value =
    let values =
      List.map (function HValue v -> Some v | _ -> None) heap_values
    in
    match values with
    | Some _ :: _ -> HValue (values |> List.map Option.get |> f)
    | None :: _ ->
        assert (List.for_all Option.is_none values);
        List.hd heap_values
    | [] -> assert false
  in

  {
    (* TODO don't statrt with shape *)
    shape with
    heap =
      zip_map_map f_heap_value Heap.seq_of_old Heap.old_of_seq
        (List.map (fun state -> state.heap) states);
    (* TODO map other fields *)
    (* TODO update vector size *)
  }

let rec normalize_value_for_shape = function
  | Scalar (SNumber _) -> Scalar (SNumber (Pico_number.of_int 0))
  | Scalar (SBool _) -> Scalar (SBool false)
  | Scalar SUnknownBool -> Scalar (SBool false)
  | Scalar (SString _) -> Scalar (SString "")
  | Scalar (SNil hint) -> Scalar (SNil hint)
  | Scalar (SPointer heap_id) -> Scalar (SPointer heap_id)
  | Scalar (SNilPointer hint) -> Scalar (SNilPointer hint)
  | Vector vec ->
      let scalar_example, _ =
        vec |> seq_of_vector |> Seq.uncons |> Option.get
      in
      normalize_value_for_shape (Scalar scalar_example)

let shape_of_normalized_state (state : state) : state =
  let shape = map_state_values normalize_value_for_shape state in
  { shape with vector_size = -1 }

let are_all_list_values_equal (l : 'a list) : bool =
  match l with
  | [] -> true
  | first :: rest -> List.for_all (fun v -> v = first) rest

let vectorize_states (states : LazyStateSet.t) : LazyStateSet.t =
  let vectorize_values (values : value list) : value =
    let example_value = List.hd values in
    if can_vectorize_value example_value then
      Vector
        (values |> List.to_seq
        |> Seq.concat_map seq_of_value
        |> vector_of_non_empty_seq)
    else (
      assert (are_all_list_values_equal values);
      example_value)
  in
  let vectorize_same_shape_states shape states =
    let vectorized_state = zip_map_state_values vectorize_values shape states in
    let vector_size = List.fold_left (fun a c -> a + c.vector_size) 0 states in
    { vectorized_state with vector_size }
  in
  let dedup_within_vectors (state : state) : state =
    Printf.printf "TODO\n";
    state
  in
  let unvectorize_if_possible (state : state) : state =
    if state.vector_size = 1 then
      map_state_values
        (fun v ->
          match v |> seq_of_value |> List.of_seq with
          | [ v ] -> Scalar v
          | _ -> assert false)
        state
    else state
  in

  let states_by_shape =
    Seq.fold_left
      (fun states_by_shape state ->
        let shape = shape_of_normalized_state state in
        let old_list =
          StateMap.find_opt shape states_by_shape |> Option.value ~default:[]
        in
        let new_list = state :: old_list in
        StateMap.add shape new_list states_by_shape)
      StateMap.empty
      (LazyStateSet.to_normalized_non_deduped_seq states)
  in
  states_by_shape |> StateMap.to_seq
  |> Seq.map (fun (shape, states) ->
         vectorize_same_shape_states shape states
         |> dedup_within_vectors |> unvectorize_if_possible)
  |> List.of_seq |> LazyStateSet.of_list

type prepared_cfg = {
  cfg : Ir.cfg;
  analyze :
    (Block_flow.flow_node -> StateAndMaybeReturnSet.t option) ->
    bool ->
    Block_flow.flow_node ->
    StateAndMaybeReturnSet.t option;
  is_noop : bool;
}

type fixed_env = {
  fun_defs : (Ir.fun_def * prepared_cfg) list;
  builtin_funs : (string * builtin_fun) list;
}

let empty_fixed_env : fixed_env = { fun_defs = []; builtin_funs = [] }

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

let interpret_unary_op_scalar (state : state) (op : string) (v : scalar_value) :
    scalar_value =
  match (op, v) with
  | "-", SNumber v -> SNumber (Pico_number.neg v)
  | "not", SBool v -> SBool (not v)
  | "not", SUnknownBool -> SUnknownBool
  | "#", SString v -> SNumber (Pico_number.of_int @@ String.length v)
  | "#", SPointer heap_id -> (
      let table = Heap.find heap_id state.heap in
      match table with
      | HArrayTable items ->
          SNumber (Pico_number.of_int @@ ListForArrayTable.length items)
      | HUnknownTable -> SNumber (Pico_number.of_int 0)
      | _ -> failwith @@ Printf.sprintf "Expected HArrayTable or HUnknownTable")
  | op, v ->
      failwith
      @@ Printf.sprintf "Unsupported unary op: %s %s" op (show_scalar_value v)

let interpret_unary_op state op v =
  match v with
  | Scalar v -> Scalar (interpret_unary_op_scalar state op v)
  | Vector v -> Vector (map_vector (interpret_unary_op_scalar state op) v)

let interpret_binary_op_scalar (l : scalar_value) (op : string)
    (r : scalar_value) : scalar_value =
  let is_simple_value v =
    match v with
    | SNumber _ -> true
    | SBool _ -> true
    | SUnknownBool -> false
    | SString _ -> true
    | SNil _ -> false
    | SPointer _ -> false
    | SNilPointer _ -> false
  in
  match (l, op, r) with
  | a, "==", b when is_simple_value a && is_simple_value b -> SBool (a = b)
  | a, "~=", b when is_simple_value a && is_simple_value b -> SBool (a <> b)
  | SNil _, "==", SNil _ -> SBool true
  | SNil _, "~=", SNil _ -> SBool false
  | a, "==", SNil _ when is_simple_value a -> SBool false
  | a, "~=", SNil _ when is_simple_value a -> SBool true
  | SNil _, "==", b when is_simple_value b -> SBool false
  | SNil _, "~=", b when is_simple_value b -> SBool true
  | SPointer l, "==", SPointer r -> SBool (l = r)
  | SPointer l, "~=", SPointer r -> SBool (l <> r)
  | a, "==", SPointer _ when is_simple_value a -> SBool false
  | a, "~=", SPointer _ when is_simple_value a -> SBool true
  | SPointer _, "==", b when is_simple_value b -> SBool false
  | SPointer _, "~=", b when is_simple_value b -> SBool true
  | SNil _, "==", SPointer _ -> SBool false
  | SNil _, "~=", SPointer _ -> SBool true
  | SPointer _, "==", SNil _ -> SBool false
  | SPointer _, "~=", SNil _ -> SBool true
  | SNumber l, "+", SNumber r -> SNumber (Pico_number.add l r)
  | SNumber l, "-", SNumber r -> SNumber (Pico_number.sub l r)
  | SNumber l, "*", SNumber r -> SNumber (Pico_number.mul l r)
  | SNumber l, "/", SNumber r -> SNumber (Pico_number.div l r)
  | SNumber l, "%", SNumber r -> SNumber (Pico_number.modulo l r)
  | SNumber l, "<", SNumber r -> SBool (Int32.compare l r < 0)
  | SNumber l, "<=", SNumber r -> SBool (Int32.compare l r <= 0)
  | SNumber l, ">", SNumber r -> SBool (Int32.compare l r > 0)
  | SNumber l, ">=", SNumber r -> SBool (Int32.compare l r >= 0)
  | SString l, "..", SString r -> SString (l ^ r)
  | SString l, "..", SNumber r ->
      SString (l ^ Int.to_string @@ Pico_number.int_of r)
  | SNumber l, "..", SString r ->
      SString ((Int.to_string @@ Pico_number.int_of l) ^ r)
  | l, op, r ->
      failwith
      @@ Printf.sprintf "Unsupported binary op: %s %s %s" (show_scalar_value l)
           op (show_scalar_value r)

let interpret_binary_op l op r =
  match (l, r) with
  | Scalar l, Scalar r -> Scalar (interpret_binary_op_scalar l op r)
  | Scalar l, Vector r ->
      Vector (map_vector (fun r -> interpret_binary_op_scalar l op r) r)
  | Vector l, Scalar r ->
      Vector (map_vector (fun l -> interpret_binary_op_scalar l op r) l)
  | Vector l, Vector r ->
      Vector (map2_vector (fun l r -> interpret_binary_op_scalar l op r) l r)

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
            {
              state with
              heap = Heap.add heap_id (HValue (Scalar (SNil None))) state.heap;
            }
          in
          (state, Scalar (SPointer heap_id)))
  | GetGlobal (name, create_if_missing) ->
      handle_separately_no_phi (fun state ->
          match StringMap.find_opt name state.global_env with
          | Some heap_id -> (state, Scalar (SPointer heap_id))
          | None when create_if_missing ->
              let state, heap_id =
                state_heap_add state (HValue (Scalar (SNil None)))
              in
              let state =
                {
                  state with
                  global_env = StringMap.add name heap_id state.global_env;
                }
              in
              (state, Scalar (SPointer heap_id))
          | None ->
              (state, Scalar (SNilPointer (Printf.sprintf "global %s" name))))
  | Load local_id ->
      handle_separately_no_phi (fun state ->
          match Ir.LocalIdMap.find local_id state.local_env with
          | Scalar (SPointer heap_id) -> (
              match Heap.find heap_id state.heap with
              | HValue value -> (state, value)
              | _ ->
                  failwith
                    "Value is of a type that can not be stored in a local")
          | Scalar (SNilPointer hint) ->
              ( state,
                Scalar (SNil (Some (Printf.sprintf "nil pointer to %s" hint)))
              )
          | v -> failwith_not_pointer v)
  | Store (target_local_id, source_local_id) ->
      handle_separately_no_phi (fun state ->
          let heap_id = heap_id_from_pointer_local state target_local_id in
          let source_value =
            Ir.LocalIdMap.find source_local_id state.local_env
          in
          let state =
            state_heap_update state (fun _ -> HValue source_value) heap_id
          in
          (state, Scalar (SNil None)))
  | StoreEmptyTable local_id ->
      handle_separately_no_phi (fun state ->
          let heap_id = heap_id_from_pointer_local state local_id in
          let state =
            state_heap_update state (fun _ -> HUnknownTable) heap_id
          in
          (state, Scalar (SNil None)))
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
          (state, Scalar (SNil None)))
  | GetField (table_local_id, field_name, create_if_missing) ->
      handle_separately_no_phi (fun state ->
          let table_heap_id = heap_id_from_pointer_local state table_local_id in
          let old_fields =
            match Heap.find table_heap_id state.heap with
            | HObjectTable old_fields -> old_fields
            | HUnknownTable -> []
            | _ ->
                failwith
                  "GetField called on something that's not an object-like \
                   table or unknown table"
          in
          match List.assoc_opt field_name old_fields with
          | Some field_heap_id -> (state, Scalar (SPointer field_heap_id))
          | None when create_if_missing ->
              let state, field_heap_id =
                state_heap_add state (HValue (Scalar (SNil None)))
              in
              let state =
                state_heap_update state
                  (fun _ ->
                    HObjectTable ((field_name, field_heap_id) :: old_fields))
                  table_heap_id
              in
              (state, Scalar (SPointer field_heap_id))
          | None ->
              ( state,
                Scalar (SNilPointer (Printf.sprintf "field %s" field_name)) ))
  | GetIndex (table_local_id, index_local_id, create_if_missing) ->
      handle_separately_no_phi (fun state ->
          let table_heap_id = heap_id_from_pointer_local state table_local_id in
          let index =
            match Ir.LocalIdMap.find index_local_id state.local_env with
            | Scalar (SNumber i) ->
                if Pico_number.fraction_int_of i <> 0 then
                  failwith "Index is a scalar number, but not an integer";
                Pico_number.int_of i
            | _ -> failwith "Index is not a scalar number"
          in
          let old_fields =
            match Heap.find table_heap_id state.heap with
            | HArrayTable old_fields -> old_fields
            | HUnknownTable -> ListForArrayTable.empty
            | _ ->
                failwith
                  "GetIndex called on something that's not an array-like table \
                   or unknown table"
          in
          match ListForArrayTable.nth_opt old_fields (index - 1) with
          | Some field_heap_id -> (state, Scalar (SPointer field_heap_id))
          | None when create_if_missing ->
              if index <> ListForArrayTable.length old_fields + 1 then
                failwith "Index is not the next index in the array";
              let state, field_heap_id =
                state_heap_add state (HValue (Scalar (SNil None)))
              in
              let state =
                state_heap_update state
                  (fun _ ->
                    HArrayTable
                      (ListForArrayTable.append old_fields field_heap_id))
                  table_heap_id
              in
              (state, Scalar (SPointer field_heap_id))
          | None ->
              (state, Scalar (SNilPointer (Printf.sprintf "index %d" index))))
  | NumberConstant v ->
      handle_separately_no_phi (fun state -> (state, Scalar (SNumber v)))
  | BoolConstant v ->
      handle_separately_no_phi (fun state -> (state, Scalar (SBool v)))
  | StringConstant v ->
      handle_separately_no_phi (fun state -> (state, Scalar (SString v)))
  | NilConstant ->
      handle_separately_no_phi (fun state -> (state, Scalar (SNil None)))
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
            match Heap.find fun_heap_id state.heap with
            | HBuiltinFun name ->
                incr_mut Perf.global_counters.builtin_call;
                let builtin_fun = List.assoc name fixed_env.builtin_funs in
                [ builtin_fun state arg_values ]
            | HClosure (fun_global_id, captured_values) ->
                incr_mut Perf.global_counters.closure_call;
                let fun_def, fun_cfg =
                  List.find
                    (fun (fun_def, _) -> fun_def.Ir.name = fun_global_id)
                    fixed_env.fun_defs
                in
                if fun_cfg.is_noop then (
                  incr_mut Perf.global_counters.closure_call_noop;
                  [ (state, Scalar (SNil None)) ])
                else
                  let padded_arg_values =
                    if List.length arg_values < List.length fun_def.Ir.arg_ids
                    then
                      arg_values
                      @ List.init
                          (List.length fun_def.Ir.arg_ids
                          - List.length arg_values)
                          (fun _ -> Scalar (SNil None))
                    else if
                      List.length arg_values > List.length fun_def.Ir.arg_ids
                    then
                      BatList.take (List.length fun_def.Ir.arg_ids) arg_values
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
                      outer_local_envs =
                        state.local_env :: state.outer_local_envs;
                    }
                  in
                  let inner_result =
                    try
                      interpret_cfg
                        (LazyStateSet.of_list [ inner_state ])
                        fun_cfg
                    with err ->
                      Printf.eprintf "Error in function %s\n" fun_global_id;
                      raise err
                  in
                  inner_result
                  |> (function
                       | StateAndMaybeReturnSet.StateSet inner_states ->
                           inner_states
                           |> LazyStateSet.to_non_normalized_non_deduped_seq
                           |> Seq.map (fun inner_state ->
                                  (inner_state, Scalar (SNil None)))
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
                             vector_size = inner_state.vector_size;
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

and flow_block_phi (source_block_name : Ir.label) (target_block : Ir.block)
    (states : LazyStateSet.t) : LazyStateSet.t =
  if !debug_prints then
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
  if !debug_prints then
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
  if !debug_prints then
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
  if !debug_prints then
    Printf.printf "before flow_branch (%s)\n" @@ debug_states states;
  match terminator with
  | Ir.Br terminator_target when terminator_target = flow_target -> states
  | Ir.Cbr (local_id, true_label, false_label)
    when flow_target = true_label || flow_target = false_label ->
      LazyStateSet.filter
        (fun state ->
          match Ir.LocalIdMap.find local_id state.local_env with
          | Scalar (SBool false) | Scalar (SNil _) -> flow_target = false_label
          | Scalar SUnknownBool -> true
          | Scalar _ -> flow_target = true_label
          | Vector _ -> failwith "TODO vector cbr")
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

and interpret_cfg (states : LazyStateSet.t) (cfg : prepared_cfg) :
    StateAndMaybeReturnSet.t =
  let init = function
    | Block_flow.BeforeEntryBlock ->
        Some (StateAndMaybeReturnSet.StateSet states)
    | _ -> None
  in
  Perf.count_and_time Perf.global_counters.fixpoint @@ fun () ->
  Option.get @@ cfg.analyze init !debug_prints Block_flow.Return

let prepare_fixpoint (cfg : Ir.cfg) (fixed_env_ref : fixed_env ref) =
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

        let is_empty = function
          | Some v -> StateAndMaybeReturnSet.is_empty v
          | None -> true

        let is_input v = v = Block_flow.BeforeEntryBlock
        let is_output = function Block_flow.Return -> true | _ -> false
        let untimed_join = Flow.lift_join StateAndMaybeReturnSet.union

        let join a b =
          Perf.count_and_time Perf.global_counters.flow_join @@ fun () ->
          untimed_join a b

        let accumulate (accumulated : StateAndMaybeReturnSet.t option)
            (potentially_new : StateAndMaybeReturnSet.t option) :
            StateAndMaybeReturnSet.t option * StateAndMaybeReturnSet.t option =
          Perf.count_and_time Perf.global_counters.flow_accumulate @@ fun () ->
          let is_empty = function
            | Some states -> StateAndMaybeReturnSet.is_empty states
            | None -> true
          in
          if is_empty potentially_new then (accumulated, None)
          else if is_empty accumulated then (potentially_new, potentially_new)
          else
            let join_result, actually_new =
              StateAndMaybeReturnSet.union_diff
                (Option.get potentially_new)
                (Option.get accumulated)
            in
            (Some join_result, Some actually_new)

        let analyze =
          let inner =
            Block_flow.make_flow_function
              (fun source_block_name target_block ->
                lift_no_return @@ flow_block_phi source_block_name target_block)
              (fun target_block ->
                lift_no_return
                @@ flow_block_before_join live_variable_analysis target_block)
              (fun block ->
                lift_no_return @@ flow_block_post_phi !fixed_env_ref block)
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
  CfgFixpoint.prepare g

let prepare_cfg (cfg : Ir.cfg) (fixed_env_ref : fixed_env ref) : prepared_cfg =
  {
    cfg;
    analyze = prepare_fixpoint cfg fixed_env_ref;
    is_noop =
      (match cfg with
      | {
       entry = { instructions = []; terminator = _, Ir.Ret None };
       named = _;
      } ->
          true
      | _ -> false);
  }

let init (cfg : Ir.cfg) (fun_defs : Ir.fun_def list)
    (builtins : (string * builtin_fun) list) :
    prepared_cfg * fixed_env ref * state =
  let state =
    {
      heap = Heap.empty;
      local_env = Ir.LocalIdMap.empty;
      outer_local_envs = [];
      global_env = StringMap.empty;
      prints = [];
      vector_size = 1;
    }
  in
  let state =
    List.fold_left
      (fun state (name, _) ->
        let state, fun_heap_id = state_heap_add state (HBuiltinFun name) in
        let state, fun_ptr_heap_id =
          state_heap_add state (HValue (Scalar (SPointer fun_heap_id)))
        in
        if StringMap.mem name state.global_env then
          failwith "Duplicate builtin name";
        {
          state with
          global_env = StringMap.add name fun_ptr_heap_id state.global_env;
        })
      state builtins
  in
  let fixed_env_ref = ref empty_fixed_env in
  fixed_env_ref :=
    {
      fun_defs =
        List.map
          (fun (fun_def : Ir.fun_def) ->
            (fun_def, prepare_cfg fun_def.cfg fixed_env_ref))
          fun_defs;
      builtin_funs = builtins;
    };
  (prepare_cfg cfg fixed_env_ref, fixed_env_ref, state)
