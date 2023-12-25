type counters = {
  enable_printing : bool ref;
  interpret_non_phi_instruction : int ref;
  builtin_call : int ref;
  closure_call : int ref;
  gc : int ref;
  normalize_state : int ref;
  flow_join : int ref;
  flow_accumulate : int ref;
  flow_accumulate_diff : int ref;
  flow_analyze : int ref;
  fixpoint_started : int ref;
  fixpoint_created_node : int ref;
}
[@@deriving show]

let global_counters : counters =
  {
    enable_printing = ref false;
    interpret_non_phi_instruction = ref 0;
    builtin_call = ref 0;
    closure_call = ref 0;
    gc = ref 0;
    normalize_state = ref 0;
    flow_join = ref 0;
    flow_accumulate = ref 0;
    flow_accumulate_diff = ref 0;
    flow_analyze = ref 0;
    fixpoint_started = ref 0;
    fixpoint_created_node = ref 0;
  }

let print_counters () =
  Printf.printf "Performance counters:\n";
  Printf.printf "  interpret_non_phi_instruction: %d\n"
    !(global_counters.interpret_non_phi_instruction);
  Printf.printf "  builtin_call: %d\n" !(global_counters.builtin_call);
  Printf.printf "  closure_call: %d\n" !(global_counters.closure_call);
  Printf.printf "  gc: %d\n" !(global_counters.gc);
  Printf.printf "  normalize_state: %d\n" !(global_counters.normalize_state);
  Printf.printf "  flow_join: %d\n" !(global_counters.flow_join);
  Printf.printf "  flow_accumulate: %d\n" !(global_counters.flow_accumulate);
  Printf.printf "  flow_accumulate_diff: %d\n"
    !(global_counters.flow_accumulate_diff);
  Printf.printf "  flow_analyze: %d\n" !(global_counters.flow_analyze);
  Printf.printf "  fixpoint_started: %d\n" !(global_counters.fixpoint_started);
  Printf.printf "  fixpoint_created_node: %d\n"
    !(global_counters.fixpoint_created_node);
  Printf.printf "\n%!"
