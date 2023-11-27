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
      if Lua.isnil state 1 then "nil" else Option.get @@ Lua.tostring state 1
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
  let collected_prints = ref [] in
  let handle_print_from_lua : Interpreter.builtin_fun =
   fun state args ->
    let s =
      match args with
      | [ Interpreter.VString s ] -> s
      | [ Interpreter.VNumber n ] -> Int.to_string @@ Pico_number.int_of n
      | _ -> failwith "Wrong args"
    in
    collected_prints := s :: !collected_prints;
    state
  in
  let ast = Lua_parser.Parse.parse_from_string lua_code in
  let stream = Frontend.compile_top_level_ast ast in
  let cfg, fun_defs = Frontend.cfg_of_stream stream in
  let fixed_env, state =
    Interpreter.init fun_defs [ ("print", handle_print_from_lua) ]
  in
  let return_value = Interpreter.interpret_cfg fixed_env state cfg in
  if return_value <> None then failwith "Unexpected return value";
  List.rev !collected_prints

let test_lua filename =
  let lua_code =
    BatFile.with_file_in
      (BatFilename.concat "lua_tests" filename)
      BatIO.read_all
  in
  let expected_prints = run_real_lua lua_code in
  let actual_prints = run_our_lua lua_code in
  assert_string_list_equal actual_prints expected_prints

let%test_unit _ = test_lua "hello_world.lua"
let%test_unit _ = test_lua "if_scopes.lua"