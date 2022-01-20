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

type heap_value =
  | ArrayTable of array_table
  | ObjectTable of object_table
  | UnknownTable
[@@deriving show]

type state = { heap : heap_value list } [@@deriving show]
type identifier = Identifier of string [@@deriving show]

type expression =
  | ExpressionNumber of pico_number
  | ExpressionBoolean of bool
  | ExpressionTable of (identifier * expression) list
[@@deriving show]

type statement = StatmentAssignment of identifier * expression
[@@deriving show]

type program = statement list [@@deriving show]

let get_global (state : state) : object_table =
  match state.heap with
  | ObjectTable t :: _ -> t
  | _ -> raise (Failure "global table not found")

let map_global f (state : state) : state =
  match state.heap with
  | ObjectTable t :: tail -> { heap = ObjectTable (f t) :: tail }
  | _ -> raise (Failure "global table not found")

let allocate o state =
  let i = List.length state.heap in
  let state = { heap = state.heap @ [ o ] } in
  (state, Concrete (ConcreteReference i))

let rec interpret_expression (state : state) (expr : expression) :
    state * any_value =
  match expr with
  | ExpressionNumber n -> (state, Concrete (ConcreteNumber n))
  | ExpressionBoolean b -> (state, Concrete (ConcreteBoolean b))
  | ExpressionTable [] -> allocate UnknownTable state
  | ExpressionTable initializers ->
      let state, initializer_values =
        List.fold_left_map
          (fun state (_, expr) -> interpret_expression state expr)
          state initializers
      in
      let value_map =
        List.map2
          (fun (Identifier k, _) v -> (k, v))
          initializers initializer_values
        |> List.to_seq |> StringMap.of_seq
      in
      allocate (ObjectTable value_map) state

let interpret_statement (state : state) (stmt : statement) : state =
  match stmt with
  | StatmentAssignment (Identifier name, expr) ->
      let state, value = interpret_expression state expr in
      map_global (StringMap.add name value) state

let debug_program (state : state) (program : program) =
  let state =
    List.fold_left
      (fun state stmt ->
        print_endline (show_state state);
        interpret_statement state stmt)
      state program
  in
  print_endline (show_state state)

let example_program : program =
  [
    StatmentAssignment
      ( Identifier "room",
        ExpressionTable
          [
            (Identifier "x", ExpressionNumber (pico_number_of_int 0));
            (Identifier "y", ExpressionNumber (pico_number_of_int 0));
          ] );
    StatmentAssignment (Identifier "objects", ExpressionTable []);
  ]

let () =
  debug_program { heap = [ ObjectTable StringMap.empty ] } example_program
