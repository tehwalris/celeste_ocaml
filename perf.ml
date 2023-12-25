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
  interpret_non_phi_instruction : int ref;
  builtin_call : int ref;
  closure_call : int ref;
  gc : timed_counter ref;
  normalize_state : int ref;
  flow_join : timed_counter ref;
  flow_accumulate : timed_counter ref;
  flow_analyze : timed_counter ref;
  fixpoint : timed_counter ref;
  fixpoint_created_node : int ref;
}
[@@deriving show]

let global_counters : counters =
  {
    enable_printing = ref false;
    reset_at = ref @@ Mtime_clock.counter ();
    interpret_non_phi_instruction = ref 0;
    builtin_call = ref 0;
    closure_call = ref 0;
    gc = ref empty_timed_counter;
    normalize_state = ref 0;
    flow_join = ref empty_timed_counter;
    flow_accumulate = ref empty_timed_counter;
    flow_analyze = ref empty_timed_counter;
    fixpoint = ref empty_timed_counter;
    fixpoint_created_node = ref 0;
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
  Printf.printf "  interpret_non_phi_instruction: %d\n"
    !(global_counters.interpret_non_phi_instruction);
  Printf.printf "  builtin_call: %d\n" !(global_counters.builtin_call);
  Printf.printf "  closure_call: %d\n" !(global_counters.closure_call);
  Printf.printf "  gc: %s\n" @@ show_timed_counter !(global_counters.gc);
  Printf.printf "  normalize_state: %d\n" !(global_counters.normalize_state);
  Printf.printf "  flow_join: %s\n"
  @@ show_timed_counter !(global_counters.flow_join);
  Printf.printf "  flow_accumulate: %s\n"
  @@ show_timed_counter !(global_counters.flow_accumulate);
  Printf.printf "  flow_analyze: %s\n"
  @@ show_timed_counter !(global_counters.flow_analyze);
  Printf.printf "  fixpoint %s\n"
  @@ show_timed_counter !(global_counters.fixpoint);
  Printf.printf "  fixpoint_created_node: %d\n"
    !(global_counters.fixpoint_created_node);
  Printf.printf "\n%!"

let count_and_time (c : timed_counter ref) f =
  let is_top_level = Option.is_none !c.top_level_timer in
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