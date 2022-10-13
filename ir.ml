type local_id = int [@@deriving show]
type label = string [@@deriving show]

type instruction =
  | Alloc
  | GetGlobal of string
  | Load of local_id
  | Store of local_id * local_id
  | StoreEmptyTable of local_id
  | GetField of local_id * string
  | NumberConstant of Pico_number.t
  | BoolConstant of bool
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

type cfg = { entry : block; named : (label * block) list }