type local_id = int [@@deriving show]
type global_id = string [@@deriving show]
type label = string [@@deriving show]

type instruction =
  | Alloc
  | GetGlobal of string * bool
  | Load of local_id
  | Store of local_id * local_id
  | StoreEmptyTable of local_id
  | StoreClosure of local_id * global_id * local_id list
  | GetField of local_id * string * bool
  | GetIndex of local_id * local_id * bool
  | NumberConstant of Pico_number.t
  | BoolConstant of bool
  | StringConstant of string
  | NilConstant
  | Call of local_id * local_id list
  | UnaryOp of string * local_id
  | BinaryOp of local_id * string * local_id
  | Phi of (label * local_id) list
[@@deriving show]

type terminator =
  | Ret of local_id option
  | Br of label
  | Cbr of local_id * label * label
[@@deriving show]

type block = {
  instructions : (local_id * instruction) list;
  terminator : local_id * terminator;
}
[@@deriving show]

type cfg = { entry : block; named : (label * block) list } [@@deriving show]

type fun_def = {
  name : global_id;
  capture_ids : local_id list;
  arg_ids : local_id list;
  cfg : cfg;
}
[@@deriving show]

module LocalIdMap = Map.Make (struct
  type t = local_id

  let compare = Stdlib.compare
end)

let show_local_id_map show_v s =
  s |> LocalIdMap.bindings
  |> List.map (fun (k, v) -> Printf.sprintf "%d -> %s" k (show_v v))
  |> String.concat "; "

module LocalIdSet = Set.Make (struct
  type t = local_id

  let compare = Stdlib.compare
end)

let show_local_id_set s =
  s |> LocalIdSet.elements |> List.map string_of_int |> String.concat "; "

module LabelMap = Map.Make (struct
  type t = label

  let compare = Stdlib.compare
end)

let instruction_map_local_ids (f : local_id -> local_id)
    (instruction : instruction) : instruction =
  match (instruction : instruction) with
  | Alloc -> instruction
  | GetGlobal _ -> instruction
  | Load var_id -> Load (f var_id)
  | Store (var_id, val_id) -> Store (f var_id, f val_id)
  | StoreEmptyTable var_id -> StoreEmptyTable (f var_id)
  | StoreClosure (var_id, closure_id, capture_ids) ->
      StoreClosure (f var_id, closure_id, List.map f capture_ids)
  | GetField (var_id, field_name, create_if_missing) ->
      GetField (f var_id, field_name, create_if_missing)
  | GetIndex (var_id, index_id, create_if_missing) ->
      GetIndex (f var_id, f index_id, create_if_missing)
  | NumberConstant _ -> instruction
  | BoolConstant _ -> instruction
  | StringConstant _ -> instruction
  | NilConstant -> instruction
  | Call (closure_id, arg_ids) -> Call (f closure_id, List.map f arg_ids)
  | UnaryOp (op, arg_id) -> UnaryOp (op, f arg_id)
  | BinaryOp (left_id, op, right_id) -> BinaryOp (f left_id, op, f right_id)
  | Phi branches -> Phi (List.map (fun (label, id) -> (label, f id)) branches)

let terminator_map_local_ids (f : local_id -> local_id)
    (terminator : terminator) : terminator =
  match (terminator : terminator) with
  | Ret (Some id) -> Ret (Some (f id))
  | Ret None -> terminator
  | Br _ -> terminator
  | Cbr (val_id, l_true, l_false) -> Cbr (f val_id, l_true, l_false)

let cfg_map_blocks (f : block -> block) (cfg : cfg) : cfg =
  {
    entry = f cfg.entry;
    named = List.map (fun (id, block) -> (id, f block)) cfg.named;
  }

let split_block_phi_instructions (block : block) =
  let is_phi = function _, Phi _ -> true | _ -> false in
  let unwrap_phi = function
    | id, Phi v -> (id, v)
    | _ -> failwith "Not a phi instruction"
  in
  let phi_instructions, non_phi_instructions =
    BatList.span is_phi block.instructions
  in
  let phi_instructions = List.map unwrap_phi phi_instructions in
  if List.exists is_phi non_phi_instructions then
    failwith "Phi instructions are not at the beginning of the block";
  (phi_instructions, non_phi_instructions)