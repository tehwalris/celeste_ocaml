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

let print_lua_perf_counters fixed_env =
  let named_counters =
    List.map
      (fun (d, cfg) -> (d.Ir.name, !(cfg.Interpreter.counter_ref)))
      fixed_env.Interpreter.fun_defs
  in
  Perf.print_named_counters named_counters

let run_step cfg states fixed_env =
  Perf.reset_counters ();
  let states_and_maybe_returns = Interpreter.interpret_cfg states cfg in
  Perf.print_counters ();
  print_lua_perf_counters fixed_env;
  let states =
    match states_and_maybe_returns with
    | Interpreter.StateAndMaybeReturnSet.StateSet states -> states
    | Interpreter.StateAndMaybeReturnSet.StateAndReturnSet _ ->
        failwith "Unexpected return value"
  in
  states |> Interpreter.LazyStateSet.normalize |> Interpreter.vectorize_states

let print_state_summary state =
  let summary = Inspect.make_state_summary state in
  Printf.printf "%s\n" @@ Inspect.show_state_summary summary

let print_step states =
  (let states = Interpreter.LazyStateSet.normalize states in
   Printf.printf "Got %d states (%d if expanding vectors) after execution\n"
     (states |> Interpreter.LazyStateSet.to_normalized_state_set
    |> Interpreter.StateSet.cardinal)
     (states |> Interpreter.LazyStateSet.to_non_normalized_non_deduped_seq
     |> Seq.fold_left (fun acc s -> acc + s.Interpreter.vector_size) 0));
  states |> Interpreter.LazyStateSet.to_normalized_state_set
  |> Interpreter.StateSet.to_seq
  |> Seq.concat_map Interpreter.unvectorize_state
  |> Seq.map Inspect.make_state_summary
  |> Seq.fold_left
       (fun m s ->
         let old_count =
           m |> Inspect.StateSummaryMap.find_opt s |> Option.value ~default:0
         in
         Inspect.StateSummaryMap.add s (old_count + 1) m)
       Inspect.StateSummaryMap.empty
  |> Inspect.StateSummaryMap.iter (fun state_summary count ->
         Printf.printf "%d %s\n" count
         @@ Inspect.show_state_summary state_summary);
  Printf.printf "\n%!"

let () =
  let lua_code = BatFile.with_file_in "celeste-minimal.lua" BatIO.read_all in
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
  let cfg, fixed_env_ref, initial_state =
    Interpreter.init cfg fun_defs
    @@ List.concat
         [
           Builtin.level_1_builtins;
           Builtin.level_2_builtins;
           Builtin.load_level_5_builtins ();
         ]
  in

  let frame_ast =
    Lua_parser.Parse.parse_from_string
    @@ String.concat "\n" [ frame_code; "\n" ]
  in
  let frame_stream = Frontend.compile_top_level_ast frame_ast in
  let frame_cfg, frame_fun_defs = Frontend.cfg_of_stream frame_stream in
  assert (frame_fun_defs = []);
  let frame_cfg = Interpreter.prepare_cfg frame_cfg fixed_env_ref in

  let states = ref @@ Interpreter.LazyStateSet.of_list [ initial_state ] in
  states := run_step cfg !states !fixed_env_ref;
  print_step !states;
  for i = 1 to 100 do
    Printf.printf "Frame %d\n%!" i;
    states := run_step frame_cfg !states !fixed_env_ref;
    print_step !states
  done
