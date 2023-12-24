open Compiler_lib

let () =
  let ast = Frontend.load_program "celeste-standard-syntax.lua" in
  let stream = Frontend.compile_top_level_ast ast in
  let cfg, fun_defs = Frontend.cfg_of_stream stream in
  let fixed_env, initial_state =
    Interpreter.init fun_defs
    @@ List.concat
         [
           Builtin.level_1_builtins;
           Builtin.level_2_builtins;
           Builtin.level_3_builtins;
         ]
  in
  let states_and_maybe_returns =
    Interpreter.interpret_cfg fixed_env
      (Interpreter.StateSet.singleton initial_state)
      cfg
  in
  let states =
    match states_and_maybe_returns with
    | Interpreter.StateAndMaybeReturnSet.StateSet states -> states
    | Interpreter.StateAndMaybeReturnSet.StateAndReturnSet _ ->
        failwith "Unexpected return value"
  in
  Printf.printf "Got %d states after execution\n"
  @@ Interpreter.StateSet.cardinal states
