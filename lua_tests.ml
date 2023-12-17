open Lua_api

let check_lua_status state status =
  match status with
  | Lua.LUA_OK -> ()
  | _ ->
      let err_msg =
        match Lua.tostring state (-1) with
        | Some msg ->
            Lua.pop state 1;
            msg
        | None -> "Unknown error"
      in
      failwith err_msg

let assert_string_list_equal l_actual l_expected =
  let format_list l = String.concat "\n" @@ List.map (Printf.sprintf "%S") l in
  if l_actual <> l_expected then (
    Printf.printf
      "Lists of strings are different.\n\nActual: \n%s\n\nExpected: \n%s"
      (format_list l_actual) (format_list l_expected);
    assert false)

let run_real_lua lua_code =
  let collected_prints = ref [] in
  let handle_print_from_lua state =
    if Lua.gettop state <> 1 then failwith "Wrong number of arguments";
    let s =
      if Lua.isnil state 1 then "nil"
      else if Lua.isboolean state 1 then
        if Lua.toboolean state 1 then "true" else "false"
      else Option.get @@ Lua.tostring state 1
    in
    collected_prints := s :: !collected_prints;
    0
  in

  let state = LuaL.newstate () in
  LuaL.openlibs state;
  Lua.pushcfunction state handle_print_from_lua;
  Lua.setglobal state "print";
  check_lua_status state @@ LuaL.loadstring state lua_code;
  check_lua_status state @@ Lua.pcall state 0 0 0;
  List.rev !collected_prints

let run_our_lua lua_code =
  let handle_print_from_lua : Interpreter.builtin_fun =
   fun state args ->
    let s =
      match args with
      | [ Interpreter.VNumber n ] -> Int.to_string @@ Pico_number.int_of n
      | [ Interpreter.VBool b ] -> if b then "true" else "false"
      | [ Interpreter.VString s ] -> s
      | [ Interpreter.VNil ] -> "nil"
      | _ -> failwith "Wrong args"
    in
    { state with Interpreter.prints = s :: state.prints }
  in
  let ast = Lua_parser.Parse.parse_from_string lua_code in
  let stream = Frontend.compile_top_level_ast ast in
  let cfg, fun_defs = Frontend.cfg_of_stream stream in
  let fixed_env, state =
    Interpreter.init fun_defs [ ("print", handle_print_from_lua) ]
  in
  let state, return_value =
    Interpreter.interpret_cfg_single_state fixed_env state cfg
  in
  if return_value <> None then failwith "Unexpected return value";
  List.rev state.Interpreter.prints

let test_lua filename =
  let lua_code =
    BatFile.with_file_in
      (BatFilename.concat "lua_tests" filename)
      BatIO.read_all
  in
  let expected_prints = run_real_lua lua_code in
  let actual_prints = run_our_lua lua_code in
  assert_string_list_equal actual_prints expected_prints

let%test_unit _ = test_lua "every_kind_of_if_else.lua"
let%test_unit _ = test_lua "hello_world.lua"
let%test_unit _ = test_lua "if_scopes.lua"
let%test_unit _ = test_lua "normal_operators.lua"
let%test_unit _ = test_lua "properties.lua"
let%test_unit _ = test_lua "scopes.lua"
let%test_unit _ = test_lua "short_circuit_operators.lua"

let find_all_regex_matches pattern text =
  let rec loop acc start =
    match Re.exec_opt ~pos:start pattern text with
    | Some group ->
        let matched = Re.Group.get group 0 in
        let match_end = Re.Group.stop group 0 in
        loop (matched :: acc) match_end
    | None -> acc
  in
  List.rev @@ loop [] 0

let parse_expected_outputs text =
  let group_pattern =
    Re.Perl.compile_pat ~opts:[ `Multiline ]
      {|^-- Expected output option:(?:\n-- .*)*|}
  in

  let strip_comment_start s =
    let prefix = "-- " in
    assert (String.starts_with ~prefix s);
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  in

  let lines_from_group s =
    s |> String.split_on_char '\n' |> List.tl |> List.map strip_comment_start
  in

  text |> find_all_regex_matches group_pattern |> List.map lines_from_group

let%test "regex thing" =
  let text =
    {|
function f(v)
  if v then
    print("a")
  else
    print("b")
  end
  if v then
    print("c")
  else
    print("d")
  end
end

f(__new_unknown_boolean())

-- Expected output option:
-- a
-- c

-- Expected output option:
-- a
-- d

-- Expected output option:
-- b
-- c

-- Expected output option:
-- b
-- d
  |}
  in
  let text = String.trim text in
  parse_expected_outputs text
  = [ [ "a"; "c" ]; [ "a"; "d" ]; [ "b"; "c" ]; [ "b"; "d" ] ]
