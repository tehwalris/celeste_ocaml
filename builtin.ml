open Interpreter

(* Level 1 *)

let builtin_new_unknown_boolean : builtin_fun =
 fun state args ->
  assert (args = []);
  (state, SUnknownBool)

let print_to_string arg =
  match arg with
  | SNumber n -> Int.to_string @@ Pico_number.int_of n
  | SBool b -> if b then "true" else "false"
  | SString s -> s
  | SNil _ -> "nil"
  | _ -> failwith "Wrong args"

let builtin_print : builtin_fun =
 fun state args ->
  let arg = match args with [ v ] -> v | _ -> failwith "Wrong args" in
  let state = { state with prints = print_to_string arg :: state.prints } in
  (state, SNil None)

let builtin_debug : builtin_fun =
 fun state args ->
  Printf.printf "debug: %s\n%!"
  @@ String.concat " "
  @@ List.map (fun arg -> print_to_string arg) args;
  (state, SNil None)

let level_1_builtins =
  [
    ("__new_unknown_boolean", builtin_new_unknown_boolean);
    ("__print", builtin_print);
    ("__debug", builtin_debug);
  ]

(* Level 2 *)

let builtin_error : builtin_fun =
 (* Unlike real Lua this doesn't actually throw Lua-level exceptions, but it still
    crashes the program, which is all we care about. *)
 fun _state args ->
  match args with
  | [] -> failwith "error called"
  | [ SString s ] -> failwith @@ Printf.sprintf "error called: %s" s
  | _ -> failwith "Wrong args"

let builtin_min : builtin_fun =
 fun state args ->
  match args with
  | [ SNumber a; SNumber b ] -> (state, SNumber (Pico_number.min a b))
  | _ -> failwith "Wrong args"

let builtin_max : builtin_fun =
 fun state args ->
  match args with
  | [ SNumber a; SNumber b ] -> (state, SNumber (Pico_number.max a b))
  | _ -> failwith "Wrong args"

let builtin_flr : builtin_fun =
 fun state args ->
  match args with
  | [ SNumber v ] -> (state, SNumber (Pico_number.flr v))
  | _ -> failwith "Wrong args"

let builtin_abs : builtin_fun =
 fun state args ->
  match args with
  | [ SNumber v ] -> (state, SNumber (Pico_number.abs v))
  | _ -> failwith "Wrong args"

let level_2_builtins =
  [
    ("error", builtin_error);
    ("min", builtin_min);
    ("max", builtin_max);
    ("flr", builtin_flr);
    ("abs", builtin_abs);
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

let builtin_mget map_data : builtin_fun =
 fun state args ->
  let x, y =
    match args with
    | [ SNumber x; SNumber y ] -> (Pico_number.int_of x, Pico_number.int_of y)
    | _ -> failwith "Wrong args"
  in
  assert (x >= 0 && x < 128);
  assert (y >= 0 && y < 64);
  let return_value =
    SNumber (Pico_number.of_int @@ Array.get map_data (x + (y * 128)))
  in
  (state, return_value)

let builtin_fget flag_data : builtin_fun =
 fun state args ->
  let i, b =
    match args with
    | [ SNumber i; SNumber b ] -> (Pico_number.int_of i, Pico_number.int_of b)
    | _ -> failwith "Wrong args"
  in
  assert (b >= 0 && b < 8);
  let v = Array.get flag_data i in
  let return_value = SBool (Int.logand v (Int.shift_left 1 b) != 0) in
  (state, return_value)

let load_level_5_builtins () =
  let map_data = load_hex_file "map-data.txt" 8192 in
  let flag_data = load_hex_file "flag-data.txt" 256 in
  [ ("mget", builtin_mget map_data); ("fget", builtin_fget flag_data) ]