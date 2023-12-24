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

let assert_string_list_list_equal l_actual l_expected =
  if List.length l_actual <> List.length l_expected then
    failwith
    @@ Printf.sprintf "Got %d lists of strings, expected %d lists of strings"
         (List.length l_actual) (List.length l_expected);
  List.iter2 assert_string_list_equal l_actual l_expected

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

let prepare_to_run_our_lua lua_code =
  (* HACK the parser does not like comments at the end of the file without a trailing newline *)
  let ast = Lua_parser.Parse.parse_from_string (lua_code ^ "\n") in
  let stream = Frontend.compile_top_level_ast ast in
  let cfg, fun_defs = Frontend.cfg_of_stream stream in
  let fixed_env, state =
    Interpreter.init fun_defs
    @@ List.concat
         [
           Builtin.level_1_builtins;
           Builtin.level_2_builtins;
           Builtin.level_3_builtins;
         ]
  in
  (cfg, fixed_env, state)

let run_our_lua_for_states lua_code =
  let cfg, fixed_env, state = prepare_to_run_our_lua lua_code in
  let states_and_maybe_returns =
    Interpreter.interpret_cfg fixed_env
      (Interpreter.StateSet.singleton state)
      cfg
  in
  let states =
    match states_and_maybe_returns with
    | Interpreter.StateAndMaybeReturnSet.StateSet states -> states
    | Interpreter.StateAndMaybeReturnSet.StateAndReturnSet _ ->
        failwith "Unexpected return value"
  in
  states

let run_our_lua_for_prints lua_code =
  lua_code |> run_our_lua_for_states |> Interpreter.StateSet.to_seq
  |> Seq.map (fun state -> List.rev state.Interpreter.prints)
  |> List.of_seq

let run_our_lua_for_prints_no_branching lua_code =
  match run_our_lua_for_prints lua_code with
  | [ result ] -> result
  | results ->
      failwith
      @@ Printf.sprintf "Expected 1 branch, got %d" (List.length results)

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

let load_lua_file filename =
  BatFile.with_file_in (BatFilename.concat "lua_tests" filename) BatIO.read_all

let test_against_real_lua filename include_level_3_for_real_lua =
  let lua_code = load_lua_file filename in
  let lua_code_for_real_lua =
    if include_level_3_for_real_lua then
      load_lua_file "_level_3.lua" ^ "\n" ^ lua_code
    else lua_code
  in
  let expected_prints = run_real_lua lua_code_for_real_lua in
  let actual_prints = run_our_lua_for_prints_no_branching lua_code in
  assert_string_list_equal actual_prints expected_prints

let test_branch_prints filename =
  let lua_code = load_lua_file filename in
  let expected_prints = parse_expected_outputs lua_code |> List.sort compare in
  let actual_prints = run_our_lua_for_prints lua_code |> List.sort compare in
  assert_string_list_list_equal actual_prints expected_prints

let test_branch_count filename expected_branch_count =
  let lua_code = load_lua_file filename in
  let states = run_our_lua_for_states lua_code in
  let actual_branch_count = Interpreter.StateSet.cardinal states in
  if actual_branch_count <> expected_branch_count then
    failwith
    @@ Printf.sprintf "Got %d branches, expected %d" actual_branch_count
         expected_branch_count

(* let%test_unit _ = test_against_real_lua "call_order.lua" false
   let%test_unit _ = test_against_real_lua "every_kind_of_if_else.lua" false *)
let%test_unit _ = test_against_real_lua "for_range.lua" true
(* let%test_unit _ = test_against_real_lua "hello_world.lua" false
   let%test_unit _ = test_against_real_lua "if_scopes.lua" false
   let%test_unit _ = test_against_real_lua "normal_operators.lua" false
   let%test_unit _ = test_against_real_lua "properties.lua" false
   let%test_unit _ = test_against_real_lua "scopes.lua" false
   let%test_unit _ = test_against_real_lua "short_circuit_operators.lua" false
   let%test_unit _ = test_against_real_lua "tables.lua" true
   let%test_unit _ = test_branch_prints "abstract_boolean_no_call.lua"
   let%test_unit _ = test_branch_prints "abstract_boolean.lua"
   let%test_unit _ = test_branch_count "branching_with_irrelevant_locals.lua" 1
   let%test_unit _ = test_branch_count "branching_with_allocations.lua" 1 *)
