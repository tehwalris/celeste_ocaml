type local_id = int [@@deriving show]
type label = string [@@deriving show]

type instruction =
  | Alloc
  | GetOrCreateGlobal of string
  | Load of local_id
  | Store of local_id * local_id
  | StoreEmptyTable of local_id
  | NumberConstant of Pico_number.t
  | BoolConstant of bool
  | Call of local_id * local_id list
[@@deriving show]

type terminator = Ret | Br of label | Cbr of local_id * label * label
[@@deriving show]

type block = {
  instructions : (local_id * instruction) list;
  terminator : local_id * terminator;
}

type cfg = { entry : block; named : (label * block) list }