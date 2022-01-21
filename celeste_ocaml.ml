open Lua_parser.Ast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(* TODO WARNING some of these pico_number functions probably have mistakes*)

type pico_number = Int32.t [@@deriving show]

let whole_int_of_pico_number (n : pico_number) : int =
  Int32.shift_right_logical n 16 |> Int32.to_int

let fraction_int_of_pico_number (n : pico_number) : int =
  Int32.shift_right_logical (Int32.shift_left n 16) 16 |> Int32.to_int

let pp_pico_number (f : Format.formatter) (n : pico_number) =
  Format.fprintf f "(pico_number %d + (%d / 65536))"
    (whole_int_of_pico_number n)
    (fraction_int_of_pico_number n)

let pico_number_of_ints (whole_n : int) (fraction_n : int) : pico_number =
  let pico_n =
    Int32.logor
      (Int32.shift_left (Int32.of_int whole_n) 16)
      (Int32.of_int fraction_n)
  in
  assert (whole_int_of_pico_number pico_n == whole_n);
  assert (fraction_int_of_pico_number pico_n == fraction_n);
  pico_n

let pico_number_of_int (n : int) : pico_number = pico_number_of_ints n 0

let rec pico_number_of_float (n : float) : pico_number =
  assert (n >= 0.);
  pico_number_of_ints (int_of_float n) (int_of_float ((n -. floor n) *. 32767.))

let pico_number_of_string n = n |> float_of_string |> pico_number_of_float

let int_of_pico_number (n : pico_number) : int =
  assert (fraction_int_of_pico_number n == 0);
  whole_int_of_pico_number n

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

let get_by_scope (name : string) (state : state) : int * string * any_value =
  let ref = Option.value (resolve_scope name state.scopes) ~default:0 in
  let value =
    match List.nth state.heap ref with
    | ObjectTable scope -> StringMap.find_opt name scope
    | _ -> failwith "scope references something that's not an ObjectTable"
  in
  let value = Option.value value ~default:(Concrete ConcreteNil) in
  (ref, name, value)

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
  (state, None, Concrete (ConcreteReference i))

let without_lhs f state expr =
  f state expr |> fun (state, lhs_value, value) -> (state, value)

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

let rec interpret_expression (state : state) (expr : ast) :
    state * (int * string) option * any_value =
  match expr with
  | Number n ->
      (state, None, Concrete (ConcreteNumber (pico_number_of_string n)))
  | Bool "true" -> (state, None, Concrete (ConcreteBoolean true))
  | Bool "false" -> (state, None, Concrete (ConcreteBoolean false))
  | Bool "nil" -> (state, None, Concrete ConcreteNil)
  | Ident name ->
      let lhs_ref, lhs_name, value = get_by_scope name state in
      (state, Some (lhs_ref, lhs_name), value)
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
          (fun state (_, expr) -> without_lhs interpret_expression state expr)
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
      let state, callee_value =
        without_lhs interpret_expression state callee_expr
      in
      let state, arg_values =
        List.fold_left_map (without_lhs interpret_expression) state arg_exprs
      in
      let state, return_value = interpret_call state callee_value arg_values in
      (state, None, Option.get return_value)
  | Clist [ lhs_expr; Key2 (Ident rhs_name) ] ->
      let state, lhs_ref =
        match interpret_expression state lhs_expr with
        | state, _, Concrete (ConcreteReference ref) -> (state, ref)
        | _ ->
            failwith "property access where left value is not ConcreteReference"
      in
      let value =
        match List.nth state.heap lhs_ref with
        | ObjectTable scope -> StringMap.find_opt rhs_name scope
        | _ ->
            failwith
              "property access where left value references something that's \
               not an ObjectTable"
      in
      let value = Option.value value ~default:(Concrete ConcreteNil) in
      (state, Some (lhs_ref, rhs_name), value)
  | Binop (op, left, right) ->
      let state, left = without_lhs interpret_expression state left in
      let state, right = without_lhs interpret_expression state right in
      (state, None, interpret_binop state op left right)
  | _ ->
      Lua_parser.Pp_ast.pp_ast_show expr;
      failwith "unsupported expression"

(* TODO WARNING some of these binop handlers probably have mistakes*)
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
  | ( "/",
      Abstract (AbstractNumberRange (left_min, left_max)),
      Concrete (ConcreteNumber right) ) ->
      Abstract
        (AbstractNumberRange (Int32.div left_min right, Int32.add left_max right))
  | "*", Concrete (ConcreteNumber left), Concrete (ConcreteNumber right) ->
      Concrete (ConcreteNumber (Int32.mul left right))
  | _ ->
      failwith
        (Printf.sprintf "unsupported op: %s %s %s" (show_any_value left) op
           (show_any_value right))

and interpret_statement (state : state) (stmt : ast) : state =
  Lua_parser.Pp_lua.pp_lua stmt;
  print_endline "";
  assert (state.return == None);
  match stmt with
  | Assign (Elist [ lhs_expr ], Elist [ expr ]) ->
      let state, (lhs_ref, lhs_name) =
        match interpret_expression state lhs_expr with
        | state, Some lhs, _ -> (state, lhs)
        | _, None, _ -> failwith "assignment to non-lhs expression"
      in
      let state, value = without_lhs interpret_expression state expr in
      let update_table = function
        | ObjectTable o -> ObjectTable (StringMap.add lhs_name value o)
        | _ ->
            failwith
              "lhs of assignment references something that's not an ObjectTable"
      in
      { state with heap = map_ith lhs_ref update_table state.heap }
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
      let state, callee_value =
        without_lhs interpret_expression state callee_expr
      in
      let state, arg_values =
        List.fold_left_map (without_lhs interpret_expression) state arg_exprs
      in
      let state, _ = interpret_call state callee_value arg_values in
      state
  | Return (Elist [ expr ]) ->
      assert (is_in_call state);
      let state, value = (without_lhs interpret_expression) state expr in
      { state with return = Some (Some value) }
  | Return (Elist []) ->
      assert (is_in_call state);
      { state with return = Some None }
  | For1 (Ident i_name, i_from, i_to, Slist body) ->
      let state, i_from = (without_lhs interpret_expression) state i_from in
      let i_from = int_of_any_value i_from in
      let state, i_to = (without_lhs interpret_expression) state i_to in
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
  ignore @@ List.fold_left interpret_statement state program

let rec parse_hex_bytes (s : char list) : int list =
  match s with
  | [] -> []
  | a :: b :: tl ->
      let v =
        int_of_string ("0x" ^ ([ a; b ] |> List.to_seq |> String.of_seq))
      in
      assert (v >= 0 && v < 256);
      v :: parse_hex_bytes tl
  | _ -> failwith "uneven number of bytes"

let example_program =
  BatFile.with_file_in "celeste-standard-syntax.lua" (fun f ->
      f |> Batteries.IO.to_input_channel |> Lua_parser.Parse.parse_from_chan)

let map_data =
  BatFile.with_file_in "map-data.txt" BatIO.read_all
  |> Str.global_replace (Str.regexp "[^a-f0-9]") ""
  |> String.to_seq |> List.of_seq |> parse_hex_bytes

let () = assert (List.length map_data == 8192)

let return_from_builtin (value : any_value) (state : state) : state =
  assert (state.return == None);
  { state with return = Some (Some value) }

let builtin_print state args =
  List.iter (fun v -> print_endline (show_any_value v)) args;
  state

let builtin_add state args =
  let target_ref, value =
    match args with
    | [ Concrete (ConcreteReference target_ref); value ] -> (target_ref, value)
    | _ -> failwith "bad arguments to add"
  in
  let update_table = function
    | ArrayTable values -> ArrayTable (values @ [ value ])
    | UnknownTable -> ArrayTable [ value ]
    | _ -> failwith "expected ArrayTable or UnknownTable"
  in
  { state with heap = map_ith target_ref update_table state.heap }

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

let builtin_flr state args =
  let flr_pico (n : pico_number) : pico_number =
    pico_number_of_ints (whole_int_of_pico_number n) 0
  in
  let result =
    match args with
    | [ Concrete (ConcreteNumber v) ] -> Concrete (ConcreteNumber (flr_pico v))
    | [ Abstract (AbstractNumberRange (min, max)) ] ->
        Abstract (AbstractNumberRange (flr_pico min, flr_pico max))
    | _ -> failwith "bad arguments to flr"
  in
  return_from_builtin result state

let builtin_foreach state args =
  let table_ref, cb =
    match args with
    | [ Concrete (ConcreteReference target_ref); cb ] -> (target_ref, cb)
    | _ -> failwith "bad arguments to add"
  in
  let values =
    match List.nth state.heap table_ref with
    | ArrayTable values -> values
    | UnknownTable -> []
    | _ -> failwith "expected ArrayTable or UnknownTable"
  in
  let call_iteration state value =
    let state, _ = interpret_call state cb [ value ] in
    state
  in
  List.fold_left call_iteration state values

let builtin_mget state args =
  let x, y =
    match args with
    | [ Concrete (ConcreteNumber x); Concrete (ConcreteNumber y) ] -> (x, y)
    | _ -> failwith "bad arguments to rnd"
  in
  let x = int_of_pico_number x in
  assert (x >= 0 && x < 128);
  let y = int_of_pico_number y in
  assert (y >= 0 && y < 32);
  let v = List.nth map_data (x + (y * 128)) in
  return_from_builtin (Concrete (ConcreteNumber (pico_number_of_int v))) state

let builtin_dead state args = state

let initial_state =
  let state =
    { heap = [ ObjectTable StringMap.empty ]; scopes = []; return = None }
  in
  let state =
    List.fold_left
      (fun state (name, f) ->
        let state, ref = without_lhs allocate (Builtin f) state in
        let state = set_by_scope name ref state in
        state)
      state
      [
        ("print", builtin_print);
        ("add", builtin_add);
        ("rnd", builtin_rnd);
        ("flr", builtin_flr);
        ("foreach", builtin_foreach);
        ("mget", builtin_mget);
        ("music", builtin_dead);
      ]
  in
  state

let () = debug_program initial_state example_program
