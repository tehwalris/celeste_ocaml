type local_id = int [@@deriving show]
type label = string [@@deriving show]

type instruction =
  | Alloc
  | GetOrCreateGlobal of string
  | Load of local_id
  | Store of local_id * local_id
  | StoreEmptyTable of local_id
[@@deriving show]

type terminator = Ret | Br of label

type block = {
  instructions : (local_id * instruction) list;
  terminator : local_id * terminator;
}

type cfg = { entry : block; named : (label * block) list }