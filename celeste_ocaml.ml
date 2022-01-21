open Lua_parser.Ast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type pico_number = Int32.t [@@deriving show]

(* TODO use the top 16 bits for integers *)
let pico_number_of_int (n : int) : pico_number = Int32.of_int n
let int_of_pico_number (n : pico_number) : int = Int32.to_int n

type concrete_value =
  | ConcreteNumber of pico_number
  | ConcreteBoolean of bool
  | ConcreteReference of int
  | ConcreteNil
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
  | ExpressionIdentifier of identifier
  | ExpressionCall of expression * expression list
[@@deriving show]

and statement =
  | StatmentAssignment of identifier * expression
  | StatementLocal of identifier * expression option
  | StatementFunction of identifier * statement list
  | StatementCall of expression * expression list
  | StatementReturn of expression option
[@@deriving show] [@@deriving show]

type heap_value =
  | ArrayTable of array_table
  | ObjectTable of object_table
  | UnknownTable
  | Function of string list * ast list * scope list
  | Builtin of (state -> any_value list -> state)
[@@deriving show]

and state = {
  heap : heap_value list;
  scopes : scope list;
  return : any_value option option;
}
[@@deriving show]

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
  | ObjectTable scope ->
      StringMap.find_opt name scope
      |> Option.value ~default:(Concrete ConcreteNil)
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

let add_local (name : string) ((ref, names) : scope) : scope =
  (ref, StringSet.add name names)

let is_in_call (state : state) : bool = List.length state.scopes != 0

let pad_or_drop p n l =
  if List.length l > n then BatList.take n l
  else l @ List.init (n - List.length l) (fun _ -> p)

let int_of_any_value v =
  match v with
  | Concrete (ConcreteNumber n) -> int_of_pico_number n
  | _ -> failwith "i_from is not an integer"

let rec interpret_expression (state : state) (expr : ast) : state * any_value =
  match expr with
  | Number n ->
      ( state,
        Concrete (ConcreteNumber (n |> int_of_string |> pico_number_of_int)) )
  | Bool "true" -> (state, Concrete (ConcreteBoolean true))
  | Bool "false" -> (state, Concrete (ConcreteBoolean false))
  | Bool "nil" -> (state, Concrete ConcreteNil)
  | Ident name -> (state, get_by_scope name state)
  | Table (Elist []) -> allocate UnknownTable state
  | Table (Elist initializer_asts) ->
      let initializers =
        List.map
          (function
            | Assign (Ident name, expr) -> (name, expr)
            | _ ->
                Lua_parser.Pp_ast.pp_ast_show expr;
                failwith "unsupported table initializer")
          initializer_asts
      in
      let state, initializer_values =
        List.fold_left_map
          (fun state (_, expr) -> interpret_expression state expr)
          state initializers
      in
      let value_map =
        List.map2
          (fun (name, _) value -> (name, value))
          initializers initializer_values
        |> List.to_seq |> StringMap.of_seq
      in
      allocate (ObjectTable value_map) state
  | FunctionE (Fbody (Elist params, Slist body)) ->
      let params =
        List.map
          (function
            | Ident name -> name | _ -> "unsupported function parameter")
          params
      in
      allocate (Function (params, body, state.scopes)) state
  | Clist [ callee_expr; Args (Elist arg_exprs) ] ->
      let state, callee_value = interpret_expression state callee_expr in
      let state, arg_values =
        List.fold_left_map interpret_expression state arg_exprs
      in
      let state, return_value = interpret_call state callee_value arg_values in
      (state, Option.get return_value)
  | Binop (op, left, right) ->
      let state, left = interpret_expression state left in
      let state, right = interpret_expression state right in
      (state, interpret_binop state op left right)
  | _ ->
      Lua_parser.Pp_ast.pp_ast_show expr;
      failwith "unsupported expression"

and interpret_binop (state : state) (op : string) (left : any_value)
    (right : any_value) : any_value =
  match (op, left, right) with
  | "+", Concrete (ConcreteNumber left), Concrete (ConcreteNumber right) ->
      Concrete (ConcreteNumber (Int32.add left right))
  | ( "+",
      Concrete (ConcreteNumber left),
      Abstract (AbstractNumberRange (right_min, right_max)) ) ->
      Abstract
        (AbstractNumberRange (Int32.add left right_min, Int32.add left right_max))
  | _ ->
      failwith
        (Printf.sprintf "unsupported op: %s %s %s" (show_any_value left) op
           (show_any_value right))

and interpret_statement (state : state) (stmt : ast) : state =
  assert (state.return == None);
  match stmt with
  | Assign (Elist [ Ident name ], Elist [ expr ]) ->
      assert (not @@ String.starts_with ~prefix:"__" name);
      let state, value = interpret_expression state expr in
      set_by_scope name value state
  | Lassign (Elist [ Ident name ], expr) ->
      let state = interpret_statement state (Lnames (Elist [ Ident name ])) in
      let state =
        interpret_statement state (Assign (Elist [ Ident name ], expr))
      in
      state
  | Lnames (Elist [ Ident name ]) ->
      {
        state with
        scopes = add_local name (List.hd state.scopes) :: List.tl state.scopes;
      }
  | Function (FNlist [ Ident name ], f) ->
      interpret_statement state
        (Assign (Elist [ Ident name ], Elist [ FunctionE f ]))
  | Clist [ callee_expr; Args (Elist arg_exprs) ] ->
      let state, callee_value = interpret_expression state callee_expr in
      let state, arg_values =
        List.fold_left_map interpret_expression state arg_exprs
      in
      let state, _ = interpret_call state callee_value arg_values in
      state
  | Return (Elist [ expr ]) ->
      assert (is_in_call state);
      let state, value = interpret_expression state expr in
      { state with return = Some (Some value) }
  | Return (Elist []) ->
      assert (is_in_call state);
      { state with return = Some None }
  | For1 (Ident i_name, i_from, i_to, Slist body) ->
      let state, i_from = interpret_expression state i_from in
      let i_from = int_of_any_value i_from in
      let state, i_to = interpret_expression state i_to in
      let i_to = int_of_any_value i_to in
      assert (i_from <= i_to);
      let i_values =
        List.init
          (i_to - i_from + 1)
          (fun i -> Concrete (ConcreteNumber (pico_number_of_int (i_from + i))))
      in
      List.fold_left
        (fun state i_value ->
          let old_scopes = state.scopes in
          let state, scope_ref =
            allocate_raw
              (ObjectTable (StringMap.singleton i_name i_value))
              state
          in
          let state =
            {
              state with
              scopes = (scope_ref, StringSet.singleton i_name) :: state.scopes;
            }
          in
          let state = List.fold_left interpret_statement state body in
          { state with scopes = old_scopes })
        state i_values
  | _ ->
      Lua_parser.Pp_ast.pp_ast_show stmt;
      failwith "unsupported statement"

and interpret_call (state : state) (callee : any_value) (args : any_value list)
    : state * any_value option =
  assert (state.return == None);
  let ref =
    match callee with
    | Concrete (ConcreteReference ref) -> ref
    | _ -> failwith "callee is not a concrete reference"
  in
  let old_scopes = state.scopes in
  let state, scope_ref = allocate_raw (ObjectTable StringMap.empty) state in
  let interpret_unless_returned state stmt =
    match state.return with
    | Some _ -> state
    | None -> interpret_statement state stmt
  in
  let state =
    match List.nth state.heap ref with
    | Function (params, body, scopes) ->
        let args =
          pad_or_drop (Concrete ConcreteNil) (List.length params) args
        in
        let state =
          {
            state with
            scopes = (scope_ref, StringSet.of_list params) :: scopes;
          }
        in
        let state =
          List.fold_left2
            (fun state name value -> set_by_scope name value state)
            state params args
        in
        let state = List.fold_left interpret_unless_returned state body in
        state
    | Builtin f -> f state args
    | _ -> failwith "callee is not a function"
  in
  ( { state with scopes = old_scopes; return = None },
    Option.value state.return ~default:None )

let debug_program (state : state) (program : ast) =
  let program =
    match program with
    | Slist program -> program
    | _ -> failwith "expected SList"
  in
  let state =
    List.fold_left
      (fun state stmt ->
        print_endline (show_state state);
        print_endline "";
        Lua_parser.Pp_lua.pp_lua stmt;
        print_endline "\n";
        interpret_statement state stmt)
      state program
  in
  print_endline (show_state state)

let example_program =
  BatFile.with_file_in "celeste-standard-syntax.lua" (fun f ->
      f |> Batteries.IO.to_input_channel |> Lua_parser.Parse.parse_from_chan)

let () = Lua_parser.Pp_ast.pp_ast_show example_program

let return_from_builtin (value : any_value) (state : state) : state =
  assert (state.return == None);
  { state with return = Some (Some value) }

let builtin_print state args =
  List.iter (fun v -> print_endline (show_any_value v)) args;
  state

let builtin_add state args = failwith "TODO add"

let builtin_rnd state args =
  let max =
    match args with
    | [ Concrete (ConcreteNumber max) ] -> max
    | _ -> failwith "bad arguments to rnd"
  in
  assert (max >= pico_number_of_int 0);
  return_from_builtin
    (Abstract (AbstractNumberRange (pico_number_of_int 0, max)))
    state

let initial_state =
  let state =
    { heap = [ ObjectTable StringMap.empty ]; scopes = []; return = None }
  in
  let state =
    List.fold_left
      (fun state (name, f) ->
        let state, ref = allocate (Builtin f) state in
        let state = set_by_scope name ref state in
        state)
      state
      [ ("print", builtin_print); ("add", builtin_add); ("rnd", builtin_rnd) ]
  in
  state

let () = debug_program initial_state example_program
