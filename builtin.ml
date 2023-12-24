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

let builtin_error : builtin_fun =
 (* Unlike real Lua this doesn't actually throw Lua-level exceptions, but it still
    crashes the program, which is all we care about. *)
 fun _state args ->
  match args with
  | [] -> failwith "error called"
  | [ VString s ] -> failwith @@ Printf.sprintf "error called: %s" s
  | _ -> failwith "Wrong args"

let level_2_builtins = [ ("print", builtin_print); ("error", builtin_error) ]