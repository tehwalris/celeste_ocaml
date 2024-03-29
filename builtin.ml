open Interpreter

let make_number_number_any_return_builtin f : builtin_fun =
  let number_of_scalar = function
    | SNumber v -> v
    | _ -> failwith "Some arguments are not numbers"
  in
  let f_scalar a b = f (number_of_scalar a) (number_of_scalar b) in
  fun state args ->
    match args with
    | [ Scalar a; Scalar b ] -> [ (state, Scalar (f_scalar a b)) ]
    | [ Scalar a; Vector b ] ->
        [ (state, map_vector (fun b -> f_scalar a b) b) ]
    | [ Vector a; Scalar b ] ->
        [ (state, map_vector (fun a -> f_scalar a b) a) ]
    | [ Vector a; Vector b ] ->
        [ (state, map2_vector (fun a b -> f_scalar a b) a b) ]
    | _ -> failwith "Wrong args"

let make_number_number_builtin f : builtin_fun =
  make_number_number_any_return_builtin (fun a b -> SNumber (f a b))

let make_number_builtin f : builtin_fun =
  let number_of_scalar = function
    | SNumber v -> v
    | _ -> failwith "Some arguments are not numbers"
  in
  let f_scalar a = SNumber (f (number_of_scalar a)) in
  fun state args ->
    match args with
    | [ Scalar a ] -> [ (state, Scalar (f_scalar a)) ]
    | [ Vector a ] -> [ (state, map_vector f_scalar a) ]
    | _ -> failwith "Wrong args"

(* Level 1 *)

let builtin_new_unknown_boolean : builtin_fun =
 fun state args ->
  assert (args = []);
  [ (state, Scalar SUnknownBool) ]

let builtin_new_vector : builtin_fun =
 fun state args ->
  let table_heap_id =
    match args with [ Scalar (SPointer v) ] -> v | _ -> failwith "Wrong args"
  in
  let item_heap_ids =
    match Heap.find table_heap_id state.heap with
    | HArrayTable items -> items
    | _ -> failwith @@ Printf.sprintf "Expected HArrayTable"
  in
  if ListForArrayTable.is_empty item_heap_ids then
    failwith "Cannot make a vector with no values";
  let vec =
    item_heap_ids |> ListForArrayTable.to_seq
    |> Seq.map (fun id ->
           match Heap.find id state.heap with
           | HValue (Scalar v) -> v
           | HValue (Vector _) -> failwith "Cannot make vector of vector values"
           | _ ->
               failwith "Value is of a type that can not be stored in a local")
    |> value_of_non_empty_seq
  in
  let vector_size =
    vec |> seq_of_value (ListForArrayTable.length item_heap_ids) |> Seq.length
  in
  if vector_size = 1 then [ (state, vec) ]
  else (
    assert (state.vector_size = 1 || state.vector_size = vector_size);
    [ ({ state with vector_size }, vec) ])

let rec print_to_string debug arg =
  match arg with
  | Scalar (SNumber n) ->
      if Pico_number.fraction_int_of n = 0 then
        string_of_int @@ Pico_number.int_of n
      else if debug then Pico_number.debug_string_of n
      else
        failwith
          "Printing fractional numbers is currently only supported in debug \
           mode"
  | Scalar (SBool b) -> if b then "true" else "false"
  | Scalar (SString s) -> s
  | Scalar (SNil _) -> "nil"
  | Vector vec ->
      Printf.sprintf "V[%s]" @@ String.concat ", "
      @@ (vec |> seq_of_vector
         |> Seq.map (fun v -> print_to_string debug (Scalar v))
         |> List.of_seq)
  | _ -> failwith "Wrong args"

let builtin_print : builtin_fun =
 fun state args ->
  let arg = match args with [ v ] -> v | _ -> failwith "Wrong args" in
  let state =
    { state with prints = print_to_string false arg :: state.prints }
  in
  [ (state, Scalar (SNil None)) ]

let builtin_print_vector_sorted : builtin_fun =
 fun state args ->
  let vec =
    match args with
    | [ Vector (VNumber vec) ] -> vec
    | _ -> failwith "Wrong args"
  in
  let s =
    vec |> Array.to_list |> List.sort compare
    |> List.map (fun v -> Int.to_string @@ Pico_number.int_of v)
    |> String.concat ", "
  in
  let state = { state with prints = s :: state.prints } in
  [ (state, Scalar (SNil None)) ]

let builtin_debug : builtin_fun =
 fun state args ->
  Printf.printf "debug: %s\n%!"
  @@ String.concat " "
  @@ List.map (fun arg -> print_to_string true arg) args;
  [ (state, Scalar (SNil None)) ]

let builtin_array_table_drop_last : builtin_fun =
 fun state args ->
  let table_heap_id =
    match args with [ Scalar (SPointer v) ] -> v | _ -> failwith "Wrong args"
  in
  let item_heap_ids =
    match Heap.find table_heap_id state.heap with
    | HArrayTable items -> items
    | _ -> failwith @@ Printf.sprintf "Expected HArrayTable"
  in
  let item_heap_ids =
    match ListForArrayTable.drop_last item_heap_ids with
    | Some v -> v
    | None -> failwith "Cannot drop last element of empty array table"
  in
  let state =
    {
      state with
      heap = Heap.add table_heap_id (HArrayTable item_heap_ids) state.heap;
    }
  in
  [ (state, Scalar (SNil None)) ]

let level_1_builtins =
  [
    ("__new_unknown_boolean", builtin_new_unknown_boolean);
    ("__new_vector", builtin_new_vector);
    ("__print", builtin_print);
    ("__print_vector_sorted", builtin_print);
    ("__debug", builtin_debug);
    ("__array_table_drop_last", builtin_array_table_drop_last);
  ]

(* Level 2 *)

let builtin_error : builtin_fun =
 (* Unlike real Lua this doesn't actually throw Lua-level exceptions, but it still
    crashes the program, which is all we care about. *)
 fun _state args ->
  match args with
  | [] -> failwith "error called"
  | [ Scalar (SString s) ] -> failwith @@ Printf.sprintf "error called: %s" s
  | _ -> failwith "Wrong args"

let builtin_min = make_number_number_builtin Pico_number.min
let builtin_max = make_number_number_builtin Pico_number.max
let builtin_abs = make_number_builtin Pico_number.abs

let number_interval_of_scalar = function
  | SNumber v -> Pico_number_interval.of_number v
  | SNumberInterval v -> v
  | _ -> failwith "Some arguments are not numbers or number intervals"

let builtin_flr : builtin_fun =
  let f (Pico_number_interval.T (low, high)) =
    let low_flr = Pico_number.flr low in
    let high_flr = Pico_number.flr high in
    if low_flr <> high_flr then
      failwith "flr of interval is not a single integer";
    low_flr
  in
  let f_scalar a = SNumber (f (number_interval_of_scalar a)) in
  fun state args ->
    match args with
    | [ Scalar a ] -> [ (state, Scalar (f_scalar a)) ]
    | [ Vector a ] -> [ (state, map_vector f_scalar a) ]
    | _ -> failwith "Wrong args"

let builtin_split_by_flr : builtin_fun =
  let split_into_same_flr_intervals v_interval =
    let (Pico_number_interval.T (low, high)) = v_interval in
    let current = ref low in
    let dispenser () =
      let v = !current in
      if v > high then None
      else
        let smallest_with_next_flr =
          v |> Pico_number.flr |> Pico_number.add (Pico_number.of_int 1)
        in
        let largest_with_same_flr =
          smallest_with_next_flr |> Pico_number.below
        in
        assert (Pico_number.flr largest_with_same_flr = Pico_number.flr v);
        assert (
          Pico_number.flr smallest_with_next_flr
          = Pico_number.add (Pico_number.flr v) (Pico_number.of_int 1));
        current := smallest_with_next_flr;
        let output_interval =
          Pico_number_interval.of_numbers v
            (Pico_number.min high largest_with_same_flr)
        in
        assert (
          Pico_number_interval.contains_interval v_interval output_interval);
        Some output_interval
    in
    Seq.of_dispenser dispenser
  in
  fun state args ->
    match args with
    | [ Scalar v ] ->
        v |> number_interval_of_scalar |> split_into_same_flr_intervals
        |> Seq.map (fun v -> (state, Scalar (SNumberInterval v)))
        |> List.of_seq
    | [ Vector vec ] ->
        let vec_intervals =
          seq_of_vector vec |> Seq.map number_interval_of_scalar |> List.of_seq
        in
        let common_interval =
          List.fold_left Pico_number_interval.union (List.hd vec_intervals)
            vec_intervals
        in
        common_interval |> split_into_same_flr_intervals
        |> Seq.filter_map (fun common_same_flr_interval ->
               let intersected_intervals_seq =
                 vec_intervals |> List.to_seq
                 |> Seq.map
                      (Pico_number_interval.intersect common_same_flr_interval)
               in
               let mask =
                 intersected_intervals_seq |> Seq.map Option.is_some
                 |> Array.of_seq
               in
               let mask_true_count =
                 Array.fold_left
                   (fun acc v -> if v then acc + 1 else acc)
                   0 mask
               in
               if mask_true_count = 0 then None
               else
                 let filtered_state =
                   {
                     (state_map_values
                        (function
                          | Scalar s -> Scalar s
                          | Vector vec -> Option.get @@ filter_vector mask vec)
                        state)
                     with
                     vector_size = mask_true_count;
                   }
                 in
                 let filtered_vec =
                   intersected_intervals_seq
                   |> Seq.filter_map (function
                        | Some interval -> Some (SNumberInterval interval)
                        | None -> None)
                   |> value_of_non_empty_seq
                 in
                 Some (filtered_state, filtered_vec))
        |> List.of_seq
    | _ -> failwith "Wrong args"

let level_2_builtins =
  [
    ("error", builtin_error);
    ("min", builtin_min);
    ("max", builtin_max);
    ("abs", builtin_abs);
    ("flr", builtin_flr);
    ("__split_by_flr", builtin_split_by_flr);
  ]

(* Level 5 *)

let rec parse_hex_bytes (s : char list) : int list =
  match s with
  | [] -> []
  | a :: b :: tl ->
      let v =
        int_of_string ("0x" ^ ([ a; b ] |> List.to_seq |> String.of_seq))
      in
      assert (v >= 0 && v < 256);
      v :: parse_hex_bytes tl
  | _ -> failwith "uneven number of bytes"

let load_hex_file name size =
  let data =
    BatFile.with_file_in name BatIO.read_all
    |> Str.global_replace (Str.regexp "[^a-f0-9]") ""
    |> String.to_seq |> List.of_seq |> parse_hex_bytes |> Array.of_list
  in
  assert (Array.length data = size);
  data

let builtin_mget map_data =
  make_number_number_builtin (fun x y ->
      let x = Pico_number.int_of x in
      let y = Pico_number.int_of y in
      assert (x >= 0 && x < 128);
      assert (y >= 0 && y < 64);
      Pico_number.of_int @@ Array.get map_data (x + (y * 128)))

let builtin_fget flag_data =
  make_number_number_any_return_builtin (fun i b ->
      let i = Pico_number.int_of i in
      let b = Pico_number.int_of b in
      assert (b >= 0 && b < 8);
      let v = Array.get flag_data i in
      SBool (Int.logand v (Int.shift_left 1 b) != 0))

let load_level_5_builtins () =
  let map_data = load_hex_file "map-data.txt" 8192 in
  let flag_data = load_hex_file "flag-data.txt" 256 in
  [ ("mget", builtin_mget map_data); ("fget", builtin_fget flag_data) ]