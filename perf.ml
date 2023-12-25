type counters = {
  (* Special fields *)
  enable_printing : bool ref;
  reset_at : float ref;
  (* Normal counters *)
  interpret_non_phi_instruction : int ref;
  builtin_call : int ref;
  closure_call : int ref;
  gc : int ref;
  gc_total_time : int ref;
  normalize_state : int ref;
  flow_join : int ref;
  flow_accumulate : int ref;
  flow_accumulate_diff : int ref;
  flow_analyze : int ref;
  flow_analyze_total_time : int ref;
  fixpoint_started : int ref;
  fixpoint_created_node : int ref;
  debug_total_time : int ref;
}
[@@deriving show]

let global_counters : counters =
  {
    reset_at = ref @@ Sys.time ();
    enable_printing = ref false;
    interpret_non_phi_instruction = ref 0;
    builtin_call = ref 0;
    closure_call = ref 0;
    gc = ref 0;
    gc_total_time = ref 0;
    normalize_state = ref 0;
    flow_join = ref 0;
    flow_accumulate = ref 0;
    flow_accumulate_diff = ref 0;
    flow_analyze = ref 0;
    flow_analyze_total_time = ref 0;
    fixpoint_started = ref 0;
    fixpoint_created_node = ref 0;
    debug_total_time = ref 0;
  }

let usecs_of_span span =
  let nsecs = Mtime.Span.to_uint64_ns span in
  Int64.to_int @@ Int64.div nsecs 1000L

let secs_of_usecs usecs = float_of_int usecs /. 1_000_000.0

let print_counters () =
  Printf.printf "Performance counters (%f seconds total):\n"
    (Sys.time () -. !(global_counters.reset_at));
  Printf.printf "  interpret_non_phi_instruction: %d\n"
    !(global_counters.interpret_non_phi_instruction);
  Printf.printf "  builtin_call: %d\n" !(global_counters.builtin_call);
  Printf.printf "  closure_call: %d\n" !(global_counters.closure_call);
  Printf.printf "  gc: %d\n" !(global_counters.gc);
  Printf.printf "  gc_total_time: %f\n"
  @@ secs_of_usecs !(global_counters.gc_total_time);
  Printf.printf "  normalize_state: %d\n" !(global_counters.normalize_state);
  Printf.printf "  flow_join: %d\n" !(global_counters.flow_join);
  Printf.printf "  flow_accumulate: %d\n" !(global_counters.flow_accumulate);
  Printf.printf "  flow_accumulate_diff: %d\n"
    !(global_counters.flow_accumulate_diff);
  Printf.printf "  flow_analyze: %d\n" !(global_counters.flow_analyze);
  Printf.printf "  flow_analyze_total_time: %f\n"
  @@ secs_of_usecs !(global_counters.flow_analyze_total_time);
  Printf.printf "  fixpoint_started: %d\n" !(global_counters.fixpoint_started);
  Printf.printf "  fixpoint_created_node: %d\n"
    !(global_counters.fixpoint_created_node);
  Printf.printf "  debug_total_time: %f\n"
  @@ secs_of_usecs !(global_counters.debug_total_time);
  Printf.printf "\n%!"

let time counter f =
  let timer = Mtime_clock.counter () in
  let res = f () in
  let span = Mtime_clock.count timer in
  counter := !counter + usecs_of_span span;
  res

let count_and_time count_counter time_counter f =
  count_counter := !count_counter + 1;
  time time_counter f