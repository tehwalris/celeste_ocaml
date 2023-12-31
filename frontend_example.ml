open Compiler_lib
module StringMap = Interpreter.StringMap
module HeapIdMap = Interpreter.HeapIdMap
module HeapIdSet = Inspect.HeapIdSet

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
    fixed_env.Interpreter.fun_defs |> Hashtbl.to_seq
    |> Seq.map (fun (name, (_, cfg)) -> (name, !(cfg.Interpreter.counter_ref)))
    |> List.of_seq
  in
  Perf.print_named_counters named_counters

let make_heap_value_abstract_by_mark :
    (Interpreter.heap_value -> Interpreter.heap_value) StringMap.t =
  let funcs =
    [
      ( "player_rem_xy",
        let half = Pico_number.of_string "0.5" in
        assert (Pico_number.add half half = Pico_number.of_int 1);
        let neg_half = Pico_number.neg half in
        let wide =
          Pico_number_interval.of_numbers neg_half (Pico_number.below half)
        in
        let f_scalar = function
          | Interpreter.SNumber v ->
              assert (Pico_number_interval.contains_number wide v);
              Interpreter.SNumberInterval wide
          | Interpreter.SNumberInterval v ->
              assert (Pico_number_interval.contains_interval wide v);
              Interpreter.SNumberInterval wide
          | _ -> assert false
        in
        function
        | Interpreter.Scalar v -> Interpreter.Scalar (f_scalar v)
        | Interpreter.Vector vec -> Interpreter.map_vector f_scalar vec );
    ]
  in
  let lift f = function
    | Interpreter.HValue v -> Interpreter.HValue (f v)
    | v -> v
  in
  funcs |> List.to_seq
  |> Seq.map (fun (k, v) -> (k, lift v))
  |> StringMap.of_seq

let make_state_abstract (state : Interpreter.state) : Interpreter.state =
  let marks = Inspect.mark_heap state in
  let make_heap_value_abstract_by_heap_id =
    marks |> StringMap.to_seq
    |> Seq.filter_map (fun (mark, heap_ids) ->
           match StringMap.find_opt mark make_heap_value_abstract_by_mark with
           | Some f ->
               Some
                 (heap_ids |> HeapIdSet.to_seq
                 |> Seq.map (fun heap_id -> (heap_id, f)))
           | None -> None)
    |> Seq.concat |> HeapIdMap.of_seq
  in
  {
    state with
    Interpreter.heap =
      state.Interpreter.heap |> Interpreter.Heap.seq_of_old
      |> Seq.map (fun (heap_id, v) ->
             match
               HeapIdMap.find_opt heap_id make_heap_value_abstract_by_heap_id
             with
             | Some f -> (heap_id, f v)
             | None -> (heap_id, v))
      |> Interpreter.Heap.old_of_seq;
  }

let run_step cfg states fixed_env =
  Perf.reset_counters ();
  let states =
    states |> Interpreter.LazyStateSet.to_non_normalized_non_deduped_seq
    |> Seq.mapi (fun i state -> (i, state))
    |> Seq.concat_map (fun (i, state) ->
           Printf.printf "Processing state %d with vector size %d\n%!" i
             state.Interpreter.vector_size;
           let states_and_maybe_returns =
             Interpreter.interpret_cfg
               (Interpreter.LazyStateSet.of_list [ state ])
               cfg
           in
           match states_and_maybe_returns with
           | Interpreter.StateAndMaybeReturnSet.StateSet states ->
               states
               |> Interpreter.LazyStateSet.to_non_normalized_non_deduped_seq
           | Interpreter.StateAndMaybeReturnSet.StateAndReturnSet _ ->
               failwith "Unexpected return value")
    |> List.of_seq
  in
  Perf.print_counters ();
  print_lua_perf_counters fixed_env;
  states |> Interpreter.LazyStateSet.of_list
  |> Interpreter.LazyStateSet.to_normalized_non_deduped_seq
  |> Seq.map make_state_abstract
  |> List.of_seq |> Interpreter.LazyStateSet.of_list
  |> Interpreter.vectorize_states

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
  |> Inspect.StateSummaryMap.to_seq |> Seq.take 3
  |> Seq.iter (fun (state_summary, count) ->
         Printf.printf "%d %s\n" count
         @@ Inspect.show_state_summary state_summary);
  Printf.printf "\n%!"

let write_debug_image (i_frame : int) (states : Interpreter.LazyStateSet.t) =
  let min_coord = -32 in
  let max_coord = 144 in
  let img_size = max_coord - min_coord in

  let coord_to_pixel_index v =
    let i = v - min_coord in
    assert (i >= 0 && i < img_size);
    i
  in

  let img = Rgb24.create img_size img_size in

  for x = 0 to img_size - 1 do
    for y = 0 to img_size - 1 do
      Rgb24.set img x y { r = 0; g = 0; b = 0 }
    done
  done;

  states |> Interpreter.LazyStateSet.to_non_normalized_non_deduped_seq
  |> Seq.concat_map Interpreter.unvectorize_state
  |> Seq.iter (fun state ->
         let summary = Inspect.make_state_summary state in
         match summary.player with
         | Some { x; y } ->
             let x = coord_to_pixel_index @@ Pico_number.int_of x in
             let y = coord_to_pixel_index @@ Pico_number.int_of y in
             Rgb24.set img x y { r = 255; g = 255; b = 255 }
         | None -> ());

  Bmp.save
    (Printf.sprintf "output/frame_%04d.bmp" i_frame)
    [] (Images.Rgb24 img)

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
  let cfg, fun_defs = Frontend.compile ast in
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
  let frame_cfg, frame_fun_defs = Frontend.compile frame_ast in
  assert (frame_fun_defs = []);
  let frame_cfg = Interpreter.prepare_cfg frame_cfg fixed_env_ref in

  let states = ref @@ Interpreter.LazyStateSet.of_list [ initial_state ] in
  states := run_step cfg !states !fixed_env_ref;
  print_step !states;
  for i = 1 to 100 do
    Printf.printf "Frame %d\n%!" i;
    states := run_step frame_cfg !states !fixed_env_ref;
    print_step !states;
    write_debug_image i !states
  done
