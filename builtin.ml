open Interpreter

(* Level 1 *)

let builtin_new_unknown_boolean : builtin_fun =
 fun state args ->
  assert (args = []);
  (state, VUnknownBool)

let level_1_builtins =
  [ ("__new_unknown_boolean", builtin_new_unknown_boolean) ]

(* Level 2 *)

let builtin_print : builtin_fun =
 fun state args ->
  let s =
    match args with
    | [ VNumber n ] -> Int.to_string @@ Pico_number.int_of n
    | [ VBool b ] -> if b then "true" else "false"
    | [ VString s ] -> s
    | [ VNil ] -> "nil"
    | _ -> failwith "Wrong args"
  in
  let state = { state with prints = s :: state.prints } in
  (state, VNil)

let level_2_builtins = [ ("print", builtin_print) ]

(* Level 3 *)

let builtin_add : builtin_fun =
 fun state args ->
  let table_heap_id, value_to_add =
    match args with [ VPointer p; v ] -> (p, v) | _ -> failwith "Wrong args"
  in
  let heap_id_of_value_to_add = gen_heap_id () in
  let state =
    {
      state with
      heap =
        HeapIdMap.add heap_id_of_value_to_add (HValue value_to_add) state.heap;
    }
  in
  let old_table = HeapIdMap.find table_heap_id state.heap in
  let new_table =
    match old_table with
    | HArrayTable items -> HArrayTable (items @ [ heap_id_of_value_to_add ])
    | HUnknownTable -> HArrayTable [ heap_id_of_value_to_add ]
    | HValue _ -> failwith "Wrong type HValue"
    | HObjectTable _ -> failwith "Wrong type HObjectTable"
    | HClosure (_, _) -> failwith "Wrong type HClosure"
    | HBuiltinFun _ -> failwith "Wrong type HBuiltinFun"
  in
  let state =
    { state with heap = HeapIdMap.add table_heap_id new_table state.heap }
  in
  (state, VNil)

let level_3_builtins = [ ("add", builtin_add) ]