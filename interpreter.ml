open Compiler_lib

type heap_id = int [@@deriving show]

let gen_heap_id : unit -> heap_id =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

type value =
  | VNumber of Pico_number.t
  | VBool of bool
  | VString of string
  | VNil
  | VPointer of heap_id

type heap_value =
  | HValue of value
  | HObjectTable of (string * heap_id) list
  | HArrayTable of (int * heap_id) list
  | HUnknownTable
  | HClosure of Ir.global_id * value list

module HeapIdMap = Map.Make (struct
  type t = heap_id

  let compare = Stdlib.compare
end)

module StringMap = Map.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

type state = {
  heap : heap_value HeapIdMap.t;
  local_env : value Ir.LocalIdMap.t;
  global_env : heap_id StringMap.t;
}

let heap_id_from_pointer_local (state : state) (local_id : Ir.local_id) :
    heap_id =
  match Ir.LocalIdMap.find local_id state.local_env with
  | VPointer heap_id -> heap_id
  | _ -> failwith "Value is not a pointer"

let interpret_instruction (state : state) (insn : Ir.instruction)
    (fun_defs : Ir.fun_def list) : value * state =
  match insn with
  | Alloc ->
      let heap_id = gen_heap_id () in
      ( VPointer heap_id,
        { state with heap = HeapIdMap.add heap_id (HValue VNil) state.heap } )
  | GetGlobal (name, create_if_missing) ->
      let heap_id, state =
        match StringMap.find_opt name state.global_env with
        | Some heap_id -> (heap_id, state)
        | None ->
            if not create_if_missing then
              failwith "Global not found, but create_if_missing was false";
            let heap_id = gen_heap_id () in
            let state =
              {
                state with
                global_env = StringMap.add name heap_id state.global_env;
              }
            in
            (heap_id, state)
      in
      (VPointer heap_id, state)
  | Load local_id ->
      let heap_id = heap_id_from_pointer_local state local_id in
      let value =
        match HeapIdMap.find heap_id state.heap with
        | HValue value -> value
        | _ -> failwith "Value is of a type that can not be stored in a local"
      in
      (value, state)
  | Store (target_local_id, source_local_id) ->
      let heap_id = heap_id_from_pointer_local state target_local_id in
      let source_value = Ir.LocalIdMap.find source_local_id state.local_env in
      let state =
        {
          state with
          heap = HeapIdMap.add heap_id (HValue source_value) state.heap;
        }
      in
      (VNil, state)
  | StoreEmptyTable local_id ->
      let heap_id = heap_id_from_pointer_local state local_id in
      let state =
        { state with heap = HeapIdMap.add heap_id HUnknownTable state.heap }
      in
      (VNil, state)
  | StoreClosure (target_local_id, fun_global_id, captured_local_ids) ->
      let heap_id = heap_id_from_pointer_local state target_local_id in
      let captured_values =
        List.map
          (fun id -> Ir.LocalIdMap.find id state.local_env)
          captured_local_ids
      in
      let state =
        {
          state with
          heap =
            HeapIdMap.add heap_id
              (HClosure (fun_global_id, captured_values))
              state.heap;
        }
      in
      (VNil, state)
  | _ -> failwith "TODO instruction not implemented"