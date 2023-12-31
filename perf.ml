type timer = Mtime_clock.counter

let pp_timer (f : Format.formatter) (_timer : timer) =
  Format.fprintf f "(timer)"

type timed_counter = {
  start_count : int;
  end_count : int;
  top_level_timer : timer option;
  top_level_time : Mtime.Span.t;
  top_level_end_count : int;
}
[@@deriving show]

let empty_timed_counter =
  {
    start_count = 0;
    end_count = 0;
    top_level_timer = None;
    top_level_time = Mtime.Span.zero;
    top_level_end_count = 0;
  }

type counters = {
  (* Special fields *)
  enable_printing : bool ref;
  reset_at : timer ref;
  (* Normal counters *)
  interpret_non_phi_instruction : timed_counter ref;
  handle_separately_no_phi : timed_counter ref;
  builtin_call : int ref;
  closure_call : timed_counter ref;
  closure_call_noop : int ref;
  closure_call_prepare_inner_states : timed_counter ref;
  closure_call_process_inner_results : timed_counter ref;
  gc : timed_counter ref;
  normalize_state_maps_except_heap : timed_counter ref;
  flow_join : timed_counter ref;
  flow_accumulate : timed_counter ref;
  flow_analyze : timed_counter ref;
  flow_analyze_flow_block_phi : timed_counter ref;
  flow_analyze_flow_block_before_join : timed_counter ref;
  flow_analyze_flow_block_post_phi : timed_counter ref;
  flow_analyze_flow_branch : timed_counter ref;
  flow_analyze_flow_return : timed_counter ref;
  fixpoint : timed_counter ref;
  fixpoint_prepare : timed_counter ref;
  fixpoint_internals : timed_counter ref;
  cbr_filter : timed_counter ref;
  filter_vector : int ref;
  filter_vector_real : int ref;
  (* Counters per Lua function *)
  lua_functions : timed_counter ref list ref;
}
[@@deriving show]

let global_counters : counters =
  {
    enable_printing = ref false;
    reset_at = ref @@ Mtime_clock.counter ();
    interpret_non_phi_instruction = ref empty_timed_counter;
    handle_separately_no_phi = ref empty_timed_counter;
    builtin_call = ref 0;
    closure_call = ref empty_timed_counter;
    closure_call_noop = ref 0;
    closure_call_prepare_inner_states = ref empty_timed_counter;
    closure_call_process_inner_results = ref empty_timed_counter;
    gc = ref empty_timed_counter;
    normalize_state_maps_except_heap = ref empty_timed_counter;
    flow_join = ref empty_timed_counter;
    flow_accumulate = ref empty_timed_counter;
    flow_analyze = ref empty_timed_counter;
    flow_analyze_flow_block_phi = ref empty_timed_counter;
    flow_analyze_flow_block_before_join = ref empty_timed_counter;
    flow_analyze_flow_block_post_phi = ref empty_timed_counter;
    flow_analyze_flow_branch = ref empty_timed_counter;
    flow_analyze_flow_return = ref empty_timed_counter;
    fixpoint = ref empty_timed_counter;
    fixpoint_prepare = ref empty_timed_counter;
    fixpoint_internals = ref empty_timed_counter;
    cbr_filter = ref empty_timed_counter;
    filter_vector = ref 0;
    filter_vector_real = ref 0;
    lua_functions = ref [];
  }

let usecs_of_span span =
  let nsecs = Mtime.Span.to_uint64_ns span in
  Int64.to_int @@ Int64.div nsecs 1000L

let secs_of_span span = (float_of_int @@ usecs_of_span span) /. 1_000_000.0

let show_timed_counter (counter : timed_counter) =
  Printf.sprintf
    "start_count: %d, end_count: %d, top_level_time: %fs%s, \
     top_level_end_count: %d"
    counter.start_count counter.end_count
    (secs_of_span counter.top_level_time)
    (match counter.top_level_timer with
    | Some timer ->
        Printf.sprintf " + %fs" @@ secs_of_span @@ Mtime_clock.count timer
    | None -> "")
    counter.top_level_end_count

let print_counters () =
  Printf.printf "Performance counters (%f seconds total):\n"
  @@ secs_of_span
  @@ Mtime_clock.count !(global_counters.reset_at);
  Printf.printf "  interpret_non_phi_instruction: %s\n"
  @@ show_timed_counter !(global_counters.interpret_non_phi_instruction);
  Printf.printf "  handle_separately_no_phi: %s\n"
  @@ show_timed_counter !(global_counters.handle_separately_no_phi);
  Printf.printf "  builtin_call: %d\n" !(global_counters.builtin_call);
  Printf.printf "  closure_call: %s\n"
  @@ show_timed_counter !(global_counters.closure_call);
  Printf.printf "  closure_call_noop: %d\n" !(global_counters.closure_call_noop);
  Printf.printf "  closure_call_prepare_inner_states: %s\n"
  @@ show_timed_counter !(global_counters.closure_call_prepare_inner_states);
  Printf.printf "  closure_call_process_inner_results: %s\n"
  @@ show_timed_counter !(global_counters.closure_call_process_inner_results);
  Printf.printf "  gc: %s\n" @@ show_timed_counter !(global_counters.gc);
  Printf.printf "  normalize_state_maps_except_heap: %s\n"
  @@ show_timed_counter !(global_counters.normalize_state_maps_except_heap);
  Printf.printf "  flow_join: %s\n"
  @@ show_timed_counter !(global_counters.flow_join);
  Printf.printf "  flow_accumulate: %s\n"
  @@ show_timed_counter !(global_counters.flow_accumulate);
  Printf.printf "  flow_analyze: %s\n"
  @@ show_timed_counter !(global_counters.flow_analyze);
  Printf.printf "  flow_analyze_flow_block_phi: %s\n"
  @@ show_timed_counter !(global_counters.flow_analyze_flow_block_phi);
  Printf.printf "  flow_analyze_flow_block_before_join: %s\n"
  @@ show_timed_counter !(global_counters.flow_analyze_flow_block_before_join);
  Printf.printf "  flow_analyze_flow_block_post_phi: %s\n"
  @@ show_timed_counter !(global_counters.flow_analyze_flow_block_post_phi);
  Printf.printf "  flow_analyze_flow_branch: %s\n"
  @@ show_timed_counter !(global_counters.flow_analyze_flow_branch);
  Printf.printf "  flow_analyze_flow_return: %s\n"
  @@ show_timed_counter !(global_counters.flow_analyze_flow_return);
  Printf.printf "  fixpoint %s\n"
  @@ show_timed_counter !(global_counters.fixpoint);
  Printf.printf "  fixpoint_prepare: %s\n"
  @@ show_timed_counter !(global_counters.fixpoint_prepare);
  Printf.printf "  fixpoint_internals %s\n"
  @@ show_timed_counter !(global_counters.fixpoint_internals);
  Printf.printf "  cbr_filter: %s\n"
  @@ show_timed_counter !(global_counters.cbr_filter);
  Printf.printf "  filter_vector: %d\n" !(global_counters.filter_vector);
  Printf.printf "  filter_vector_real: %d\n"
    !(global_counters.filter_vector_real);
  Printf.printf "%!"

let print_named_counters (counters : (string * timed_counter) list) =
  let counters =
    counters
    |> List.filter (fun (_, counter) -> counter.start_count > 0)
    |> List.sort (fun (_, a) (_, b) ->
           compare b.top_level_time a.top_level_time)
  in
  Printf.printf "Named performance counters:\n";
  List.iter
    (fun (name, counter) ->
      Printf.printf "  %s: %s\n" name @@ show_timed_counter counter)
    counters;
  Printf.printf "%!"

let reset_counters () =
  global_counters.reset_at := Mtime_clock.counter ();
  global_counters.interpret_non_phi_instruction := empty_timed_counter;
  global_counters.handle_separately_no_phi := empty_timed_counter;
  global_counters.builtin_call := 0;
  global_counters.closure_call := empty_timed_counter;
  global_counters.closure_call_noop := 0;
  global_counters.closure_call_prepare_inner_states := empty_timed_counter;
  global_counters.closure_call_process_inner_results := empty_timed_counter;
  global_counters.gc := empty_timed_counter;
  global_counters.normalize_state_maps_except_heap := empty_timed_counter;
  global_counters.flow_join := empty_timed_counter;
  global_counters.flow_accumulate := empty_timed_counter;
  global_counters.flow_analyze := empty_timed_counter;
  global_counters.flow_analyze_flow_block_phi := empty_timed_counter;
  global_counters.flow_analyze_flow_block_before_join := empty_timed_counter;
  global_counters.flow_analyze_flow_block_post_phi := empty_timed_counter;
  global_counters.flow_analyze_flow_branch := empty_timed_counter;
  global_counters.flow_analyze_flow_return := empty_timed_counter;
  global_counters.fixpoint := empty_timed_counter;
  global_counters.fixpoint_prepare := empty_timed_counter;
  global_counters.fixpoint_internals := empty_timed_counter;
  global_counters.cbr_filter := empty_timed_counter;
  global_counters.filter_vector := 0;
  global_counters.filter_vector_real := 0;
  List.iter (fun c -> c := empty_timed_counter) !(global_counters.lua_functions)

let count_and_time_with_options ~(allow_recursion : bool)
    (c : timed_counter ref) f =
  let is_top_level = Option.is_none !c.top_level_timer in
  assert (allow_recursion || is_top_level);
  c := { !c with start_count = !c.start_count + 1 };
  let timer = Mtime_clock.counter () in
  if is_top_level then c := { !c with top_level_timer = Some timer };
  let res = f () in
  let span = Mtime_clock.count timer in
  c := { !c with end_count = !c.end_count + 1 };
  if is_top_level then
    c :=
      {
        !c with
        top_level_timer = None;
        top_level_time = Mtime.Span.add !c.top_level_time span;
        top_level_end_count = !c.top_level_end_count + 1;
      };
  res

let count_and_time c f = count_and_time_with_options ~allow_recursion:true c f
