open Compiler_lib

let suffix_code =
  String.trim
    {|
-- TODO double check that the order is: init, update, draw, update, draw, ...
_init()
__reset_button_states()
|}

let frame_code = String.trim {|
_update()
_draw()
__reset_button_states()
|}

let run_step fixed_env cfg states =
  Perf.reset_counters ();
  let states_and_maybe_returns =
    Interpreter.interpret_cfg fixed_env states cfg
  in
  Perf.print_counters ();
  let states =
    match states_and_maybe_returns with
    | Interpreter.StateAndMaybeReturnSet.StateSet states ->
        Interpreter.LazyStateSet.normalize states
    | Interpreter.StateAndMaybeReturnSet.StateAndReturnSet _ ->
        failwith "Unexpected return value"
  in
  states

let print_step states =
  Printf.printf "Got %d states after execution\n"
  @@ Interpreter.StateSet.cardinal
  @@ Interpreter.LazyStateSet.to_normalized_state_set states;
  Printf.printf "\n%!"

let () =
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

  let frame_ast =
    Lua_parser.Parse.parse_from_string
    @@ String.concat "\n" [ frame_code; "\n" ]
  in
  let frame_stream = Frontend.compile_top_level_ast frame_ast in
  let frame_cfg, frame_fun_defs = Frontend.cfg_of_stream frame_stream in
  assert (frame_fun_defs = []);

  let fixed_env, initial_state =
    Interpreter.init fun_defs
    @@ List.concat
         [
           Builtin.level_1_builtins;
           Builtin.level_2_builtins;
           Builtin.load_level_5_builtins ();
         ]
  in

  let states = ref @@ Interpreter.LazyStateSet.of_list [ initial_state ] in
  states := run_step fixed_env cfg !states;
  print_step !states;
  for i = 1 to 100 do
    Printf.printf "Frame %d\n" i;
    states := run_step fixed_env frame_cfg !states;
    print_step !states
  done
