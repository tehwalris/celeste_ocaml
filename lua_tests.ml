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
  let cfg, fun_defs = Frontend.compile ast in
  let cfg, fixed_env, state =
    Interpreter.init cfg fun_defs
    @@ List.concat [ Builtin.level_1_builtins; Builtin.level_2_builtins ]
  in
  (cfg, fixed_env, state)

let run_our_lua_for_states lua_code =
  let cfg, _, state = prepare_to_run_our_lua lua_code in
  let states_and_maybe_returns =
    Interpreter.interpret_cfg (Interpreter.LazyStateSet.of_list [ state ]) cfg
  in
  let states =
    match states_and_maybe_returns with
    | Interpreter.StateAndMaybeReturnSet.StateSet states ->
        Interpreter.LazyStateSet.to_normalized_state_set states
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

let test_against_real_lua filename include_level_3 =
  let lua_code = load_lua_file filename in
  let lua_code =
    if include_level_3 then
      BatFile.with_file_in "builtin_level_3.lua" BatIO.read_all
      ^ "\n" ^ lua_code
    else lua_code
  in
  let lua_code_for_real_lua = "__print = print\n" ^ lua_code in
  let actual_prints = run_our_lua_for_prints_no_branching lua_code in
  let expected_prints = run_real_lua lua_code_for_real_lua in
  assert_string_list_equal actual_prints expected_prints

let test_branch_prints filename include_level_3 =
  let lua_code = load_lua_file filename in
  let lua_code =
    if include_level_3 then
      BatFile.with_file_in "builtin_level_3.lua" BatIO.read_all
      ^ "\n" ^ lua_code
    else lua_code
  in
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

let%test_unit _ = test_against_real_lua "call_order.lua" false

let%test_unit _ =
  test_against_real_lua "call_with_different_number_of_args.lua" false

let%test_unit _ = test_against_real_lua "every_kind_of_if_else.lua" false
let%test_unit _ = test_against_real_lua "for_break.lua" false
let%test_unit _ = test_against_real_lua "for_range.lua" true
let%test_unit _ = test_against_real_lua "foreach.lua" true
let%test_unit _ = test_against_real_lua "hello_world.lua" false
let%test_unit _ = test_against_real_lua "if_scopes.lua" false

let%test_unit _ =
  test_against_real_lua "liveness_issue_for_in_function.lua" true

let%test_unit _ = test_against_real_lua "normal_operators.lua" false
let%test_unit _ = test_against_real_lua "properties.lua" false
let%test_unit _ = test_against_real_lua "scopes.lua" false
let%test_unit _ = test_against_real_lua "short_circuit_operators.lua" false

let%test_unit _ =
  test_against_real_lua "short_circuit_operators_non_boolean.lua" false

let%test_unit _ = test_against_real_lua "string_length.lua" false
let%test_unit _ = test_against_real_lua "tables.lua" true
let%test_unit _ = test_against_real_lua "undefined_fields.lua" true
let%test_unit _ = test_branch_prints "abstract_boolean_no_call.lua" false
let%test_unit _ = test_branch_prints "abstract_boolean.lua" false
let%test_unit _ = test_branch_prints "for_range_vector_both.lua" true
let%test_unit _ = test_branch_prints "for_range_vector_low.lua" true
let%test_unit _ = test_branch_prints "if_vector.lua" true
let%test_unit _ = test_branch_prints "less_than.lua" true
let%test_unit _ = test_branch_prints "vector_branch.lua" true
let%test_unit _ = test_branch_prints "vector.lua" true
let%test_unit _ = test_branch_count "branching_with_irrelevant_locals.lua" 1
let%test_unit _ = test_branch_count "branching_with_allocations.lua" 1
let%test_unit _ = test_branch_count "branching_into_return.lua" 1

let%test "prepared_cfg.is_noop" =
  let lua_code = "function f() end\n" in
  let ast = Lua_parser.Parse.parse_from_string lua_code in
  let stream = Frontend.compile_top_level_ast ast in
  let cfg =
    match Frontend.cfg_of_stream stream with
    | _, [ { cfg; _ } ], _ -> cfg
    | _ -> failwith "expected one function"
  in
  let cfg = Interpreter.prepare_cfg cfg (ref Interpreter.empty_fixed_env) in
  cfg.is_noop

let%test_unit "vectorize" =
  let interpret_no_return cfg states =
    match Interpreter.interpret_cfg states cfg with
    | Interpreter.StateAndMaybeReturnSet.StateSet states ->
        Interpreter.LazyStateSet.to_normalized_state_set states
    | Interpreter.StateAndMaybeReturnSet.StateAndReturnSet _ -> assert false
  in

  let lua_code_pre = load_lua_file "vectorize_pre.lua" in
  let cfg_pre, fixed_env_ref, inital_state =
    prepare_to_run_our_lua lua_code_pre
  in
  let states =
    interpret_no_return cfg_pre
    @@ Interpreter.LazyStateSet.of_list [ inital_state ]
  in
  assert (Interpreter.StateSet.cardinal states = 3);

  let assert_after_vectorize (states : Interpreter.LazyStateSet.t) =
    let states = Interpreter.vectorize_states states in
    assert (Interpreter.LazyStateSet.cardinal_upper_bound states = 1);
    assert (
      let only_state =
        states |> Interpreter.LazyStateSet.to_normalized_non_deduped_seq
        |> Seq.uncons |> Option.get |> fst
      in
      only_state.vector_size = 3);

    let lua_code_post = load_lua_file "vectorize_post.lua" in
    let ast_post = Lua_parser.Parse.parse_from_string (lua_code_post ^ "\n") in
    let cfg_post, fun_defs_post = Frontend.compile ast_post in
    assert (fun_defs_post = []);
    let cfg_post = Interpreter.prepare_cfg cfg_post fixed_env_ref in

    let states = interpret_no_return cfg_post states in
    assert (Interpreter.StateSet.cardinal states = 1);

    let expected_prints =
      parse_expected_outputs lua_code_post |> List.sort compare
    in
    let actual_prints =
      states |> Interpreter.StateSet.to_seq
      |> Seq.map (fun state -> List.rev state.Interpreter.prints)
      |> List.of_seq
    in
    assert_string_list_list_equal actual_prints expected_prints
  in

  assert_after_vectorize @@ Interpreter.LazyStateSet.of_list @@ List.of_seq
  @@ Interpreter.StateSet.to_seq states;
  assert_after_vectorize @@ Interpreter.LazyStateSet.of_list @@ List.concat
  @@ List.map
       (fun s -> s |> Interpreter.StateSet.to_seq |> List.of_seq)
       [ states; states ]
