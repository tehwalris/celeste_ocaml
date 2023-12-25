open Compiler_lib

let suffix_code = String.trim {|
_init()
|}

let () =
  (* Perf.global_counters.enable_printing := true; *)
  let lua_code =
    BatFile.with_file_in "celeste-standard-syntax.lua" BatIO.read_all
  in
  let ast =
    Lua_parser.Parse.parse_from_string
    @@ String.concat "\n"
         [
           BatFile.with_file_in "builtin_level_3.lua" BatIO.read_all;
           BatFile.with_file_in "builtin_level_4.lua" BatIO.read_all;
           lua_code;
           suffix_code;
           "\n";
         ]
  in
  let stream = Frontend.compile_top_level_ast ast in
  let cfg, fun_defs = Frontend.cfg_of_stream stream in
  let fixed_env, initial_state =
    Interpreter.init fun_defs
    @@ List.concat
         [
           Builtin.level_1_builtins;
           Builtin.level_2_builtins;
           Builtin.load_level_5_builtins ();
         ]
  in
  let states_and_maybe_returns =
    Interpreter.interpret_cfg fixed_env
      (Interpreter.StateSet.singleton initial_state)
      cfg
  in
  Perf.print_counters ();
  let states =
    match states_and_maybe_returns with
    | Interpreter.StateAndMaybeReturnSet.StateSet states -> states
    | Interpreter.StateAndMaybeReturnSet.StateAndReturnSet _ ->
        failwith "Unexpected return value"
  in
  Printf.printf "Got %d states after execution\n"
  @@ Interpreter.StateSet.cardinal states
