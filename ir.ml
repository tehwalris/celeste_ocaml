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
