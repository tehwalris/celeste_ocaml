module HeapIdSet = Set.Make (struct
  type t = Interpreter.heap_id

  let compare = compare
end)

module StringMap = Interpreter.StringMap

let unwrap_pointer = function
  | Interpreter.HValue (Interpreter.Scalar (Interpreter.SPointer id)) -> id
  | _ -> failwith "not a pointer"

type state_helper = {
  find_global : string -> int;
  load : int -> Interpreter.heap_value;
  load_array_table : int -> Interpreter.heap_value list;
  load_object_table : int -> Interpreter.heap_value StringMap.t;
  find_objects_by_type : int -> int list;
}

let make_state_helper state =
  let find_global name = StringMap.find name state.Interpreter.global_env in
  let load heap_id = Interpreter.Heap.find heap_id state.Interpreter.heap in
  let load_array_table table_heap_id =
    match load table_heap_id with
    | Interpreter.HArrayTable l ->
        l |> Interpreter.ListForArrayTable.to_seq |> Seq.map load |> List.of_seq
    | Interpreter.HUnknownTable -> []
    | _ -> failwith "Expected HArrayTable or HUnknownTable"
  in
  let load_object_table table_heap_id =
    match load table_heap_id with
    | Interpreter.HObjectTable l ->
        l |> List.to_seq
        |> Seq.map (fun (k, v) -> (k, load v))
        |> StringMap.of_seq
    | Interpreter.HUnknownTable -> StringMap.empty
    | _ -> failwith "Expected HObjectTable or HUnknownTable"
  in

  let find_objects_by_type type_heap_id =
    find_global "objects" |> load_array_table |> List.map unwrap_pointer
    |> List.filter (fun object_heap_id ->
           let obj = load_object_table object_heap_id in
           unwrap_pointer (StringMap.find "type" obj) = type_heap_id)
  in

  {
    find_global;
    load;
    load_array_table;
    load_object_table;
    find_objects_by_type;
  }

let mark_heap state =
  let marks = ref StringMap.empty in
  let mark k heap_id =
    let old_heap_ids =
      !marks |> StringMap.find_opt k |> Option.value ~default:HeapIdSet.empty
    in
    marks := StringMap.add k (HeapIdSet.add heap_id old_heap_ids) !marks
  in

  let h = make_state_helper state in

  h.find_global "player" |> h.find_objects_by_type |> List.iter (mark "player");

  !marks

let list_one_or_none l =
  match l with
  | [ x ] -> Some x
  | [] -> None
  | l ->
      failwith
      @@ Printf.sprintf "Expected 0 or 1 elements, got %d"
      @@ List.length l

let heap_id_set_one_or_none s =
  match HeapIdSet.elements s with
  | [ x ] -> Some x
  | [] -> None
  | l ->
      failwith
      @@ Printf.sprintf "Expected 0 or 1 elements, got %d"
      @@ List.length l

let heap_id_set_exactly_one s = Option.get @@ heap_id_set_one_or_none s

let number_of_heap_value = function
  | Interpreter.HValue (Interpreter.Scalar (Interpreter.SNumber n)) -> n
  | _ -> failwith "expected scalar number"

type player_state_summary = { x : Pico_number.t; y : Pico_number.t }
[@@deriving show]

type state_summary = { player : player_state_summary option } [@@deriving show]

let make_state_summary state : state_summary =
  let h = make_state_helper state in

  let make_player_state_summary player_heap_id =
    let player = h.load_object_table player_heap_id in
    {
      x = StringMap.find "x" player |> number_of_heap_value;
      y = StringMap.find "y" player |> number_of_heap_value;
    }
  in

  let player_heap_id =
    h.find_global "player" |> h.find_objects_by_type |> list_one_or_none
  in

  { player = Option.map make_player_state_summary player_heap_id }
