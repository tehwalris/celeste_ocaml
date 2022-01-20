module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

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

type scope = int * StringSet.t

let pp_scope (f : Format.formatter) ((ref, values) : scope) =
  Format.fprintf f "{ %d %s }" ref
    (Seq.fold_left (fun a b -> a ^ "; " ^ b) "" (StringSet.to_seq values))

type identifier = Identifier of string [@@deriving show]

type expression =
  | ExpressionNumber of pico_number
  | ExpressionBoolean of bool
  | ExpressionTable of (identifier * expression) list
  | ExpressionFunction of statement list
[@@deriving show]

and statement =
  | StatmentAssignment of identifier * expression
  | StatementLocal of identifier * expression option
  | StatementFunction of identifier * statement list
  | StatementCall of identifier
[@@deriving show] [@@deriving show]

type heap_value =
  | ArrayTable of array_table
  | ObjectTable of object_table
  | UnknownTable
  | Function of statement list * scope list
[@@deriving show]

type state = { heap : heap_value list; scopes : scope list } [@@deriving show]
type program = statement list [@@deriving show]

let resolve_scope (name : string) (scopes : scope list) : int option =
  scopes
  |> List.find_opt (fun (_, names) -> StringSet.mem name names)
  |> Option.map (fun (ref, _) -> ref)

let map_ith i cb l =
  assert (i >= 0 && i < List.length l);
  List.mapi (fun j v -> if i == j then cb v else v) l

let get_by_scope (name : string) (state : state) : any_value =
  let ref = Option.value (resolve_scope name state.scopes) ~default:0 in
  match List.nth state.heap ref with
  | ObjectTable scope -> StringMap.find name scope
  | _ -> failwith "scope references something that's not an ObjectTable"

let set_by_scope (name : string) (value : any_value) (state : state) : state =
  let update_table = function
    | ObjectTable o -> ObjectTable (StringMap.add name value o)
    | _ -> failwith "scope references something that's not an ObjectTable"
  in
  let ref = Option.value (resolve_scope name state.scopes) ~default:0 in
  { state with heap = map_ith ref update_table state.heap }

let allocate_raw o state =
  let i = List.length state.heap in
  let state = { state with heap = state.heap @ [ o ] } in
  (state, i)

let allocate o state =
  let state, i = allocate_raw o state in
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
  | ExpressionFunction body -> allocate (Function (body, state.scopes)) state

let add_local (name : string) ((ref, names) : scope) : scope =
  (ref, StringSet.add name names)

let rec interpret_statement (state : state) (stmt : statement) : state =
  match stmt with
  | StatmentAssignment (Identifier name, expr) ->
      let state, value = interpret_expression state expr in
      set_by_scope name value state
  | StatementLocal (name, Some expr) ->
      let state = interpret_statement state (StatementLocal (name, None)) in
      let state = interpret_statement state (StatmentAssignment (name, expr)) in
      state
  | StatementLocal (Identifier name, None) ->
      {
        state with
        scopes = add_local name (List.hd state.scopes) :: List.tl state.scopes;
      }
  | StatementFunction (name, body) ->
      interpret_statement state
        (StatmentAssignment (name, ExpressionFunction body))
  | StatementCall (Identifier name) ->
      let ref =
        match get_by_scope name state with
        | Concrete (ConcreteReference ref) -> ref
        | _ -> failwith "callee is not a concrete reference"
      in
      let old_scopes = state.scopes in
      let state, scope_ref = allocate_raw (ObjectTable StringMap.empty) state in
      let state =
        match List.nth state.heap ref with
        | Function (body, scopes) ->
            List.fold_left interpret_statement
              { state with scopes = (scope_ref, StringSet.empty) :: scopes }
              body
        | _ -> failwith "callee is not a function"
      in
      { state with scopes = old_scopes }

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
    StatmentAssignment (Identifier "x", ExpressionNumber (pico_number_of_int 0));
    StatmentAssignment (Identifier "y", ExpressionNumber (pico_number_of_int 7));
    StatementFunction
      ( Identifier "g",
        [
          StatmentAssignment
            (Identifier "y", ExpressionNumber (pico_number_of_int 6));
          StatementLocal
            (Identifier "y", Some (ExpressionNumber (pico_number_of_int 4)));
        ] );
    StatementCall (Identifier "g");
  ]

let () =
  debug_program
    { heap = [ ObjectTable StringMap.empty ]; scopes = [] }
    example_program
