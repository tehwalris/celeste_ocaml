module StringMap = Map.Make (String)

type pico_number = Int32.t [@@deriving show]

(* TODO use the top 16 bits for integers *)
let pico_number_of_int (n : int) : pico_number = Int32.of_int n

type concrete_value =
  | ConcreteNumber of pico_number
  | ConcreteBoolean of bool
  | ConcreteReference of int
[@@deriving show]

type abstract_value =
  | AbstractOneOf of concrete_value list
  | AbstractNumberRange of pico_number * pico_number
[@@deriving show]

type any_value = Concrete of concrete_value | Abstract of abstract_value
[@@deriving show]

type array_table = any_value list [@@deriving show]
type object_table = any_value StringMap.t

let pp_object_table (f : Format.formatter) (_t : object_table) =
  Format.fprintf f "{ %s }"
    (Seq.fold_left
       (fun a b -> a ^ "; " ^ b)
       ""
       (Seq.map
          (fun (k, v) -> k ^ " = " ^ show_any_value v)
          (StringMap.to_seq _t)))

type state = { global : object_table } [@@deriving show]
type identifier = Identifier of string [@@deriving show]

type expression =
  | ExpressionNumber of pico_number
  | ExpressionTable of (identifier * expression) list
[@@deriving show]

type statement = StatmentAssignment of identifier * expression
[@@deriving show]

type program = statement list [@@deriving show]

let interpret_statement (stmt : statement) (state : state) : state =
  match stmt with
  | StatmentAssignment (Identifier name, expr) ->
      let v =
        match expr with
        | ExpressionNumber n -> Concrete (ConcreteNumber n)
        | ExpressionTable _ -> raise (Failure "not implemented")
      in
      { global = StringMap.add name v state.global }

let debug_program (state : state) (program : program) =
  let state =
    List.fold_left
      (fun state stmt ->
        print_endline (show_state state);
        interpret_statement stmt state)
      state program
  in
  print_endline (show_state state)

let example_program : program =
  [
    StatmentAssignment
      (Identifier "a", ExpressionNumber (pico_number_of_int 123));
    StatmentAssignment
      (Identifier "b", ExpressionNumber (pico_number_of_int 456));
    StatmentAssignment (Identifier "a", ExpressionNumber (pico_number_of_int 0));
  ]

let () = debug_program { global = StringMap.empty } example_program