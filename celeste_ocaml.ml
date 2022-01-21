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
  assert (whole_int_of_pico_number pico_n = whole_n);
  assert (fraction_int_of_pico_number pico_n = fraction_n);
  pico_n

let pico_number_of_int (n : int) : pico_number = pico_number_of_ints n 0

let rec pico_number_of_float (n : float) : pico_number =
  assert (n >= 0.);
  pico_number_of_ints (int_of_float n) (int_of_float ((n -. floor n) *. 32767.))

let pico_number_of_string n = n |> float_of_string |> pico_number_of_float

let int_of_pico_number (n : pico_number) : int =
  assert (fraction_int_of_pico_number n = 0);
  whole_int_of_pico_number n

let equal_pico_number = Int32.equal

type concrete_value =
  | ConcreteNumber of pico_number
  | ConcreteBoolean of bool
  | ConcreteReference of int
  | ConcreteNil
[@@deriving show, eq]

type abstract_value =
  | AbstractOneOf of concrete_value list
  | AbstractNumberRange of pico_number * pico_number
[@@deriving show]

type any_value = Concrete of concrete_value | Abstract of abstract_value
[@@deriving show]

type lhs_value =
  | ArrayTableElement of int * int
  | ObjectTableElement of int * string

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

type interpreter_context = { on_statement : ast -> unit }
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
  | Builtin of (interpreter_context -> state -> any_value list -> state)
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
  List.mapi (fun j v -> if i = j then cb v else v) l

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

let rec interpret_expression (ctx : interpreter_context) (state : state)
    (expr : ast) : state * lhs_value option * any_value =
  match expr with
  | Number n ->
      (state, None, Concrete (ConcreteNumber (pico_number_of_string n)))
  | Bool "true" -> (state, None, Concrete (ConcreteBoolean true))
  | Bool "false" -> (state, None, Concrete (ConcreteBoolean false))
  | Bool "nil" -> (state, None, Concrete ConcreteNil)
  | Ident name ->
      let lhs_ref, lhs_name, value = get_by_scope name state in
      (state, Some (ObjectTableElement (lhs_ref, lhs_name)), value)
  | Table (Elist []) ->
      let state, value = allocate UnknownTable state in
      (state, None, value)
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
          (fun state (_, expr) -> interpret_rhs_expression ctx state expr)
          state initializers
      in
      let value_map =
        List.map2
          (fun (name, _) value -> (name, value))
          initializers initializer_values
        |> List.to_seq |> StringMap.of_seq
      in
      let state, value = allocate (ObjectTable value_map) state in
      (state, None, value)
  | FunctionE (Fbody (Elist params, Slist body)) ->
      let params =
        List.map
          (function
            | Ident name -> name | _ -> "unsupported function parameter")
          params
      in
      let state, value =
        allocate (Function (params, body, state.scopes)) state
      in
      (state, None, value)
  | Clist [ callee_expr; Args (Elist arg_exprs) ] ->
      let state, callee_value =
        interpret_rhs_expression ctx state callee_expr
      in
      let state, arg_values =
        List.fold_left_map (interpret_rhs_expression ctx) state arg_exprs
      in
      let state, return_value =
        interpret_call ctx state callee_value arg_values
      in
      (state, None, Option.get return_value)
  | Clist [ lhs_expr; Key1 rhs_expr ] ->
      let state, lhs_ref =
        match interpret_expression ctx state lhs_expr with
        | state, _, Concrete (ConcreteReference ref) -> (state, ref)
        | _ ->
            failwith "element access where left value is not ConcreteReference"
      in
      let state, rhs_index =
        match interpret_expression ctx state rhs_expr with
        | state, _, Concrete (ConcreteNumber i) ->
            (state, int_of_pico_number i - 1)
        | _ -> failwith "element access where right value is not ConcreteNumber"
      in
      let value =
        match List.nth state.heap lhs_ref with
        | ArrayTable values -> List.nth values rhs_index
        | _ ->
            failwith
              "element access where left value references something that's not \
               an ArrayTable"
      in
      (state, Some (ArrayTableElement (lhs_ref, rhs_index)), value)
  | Clist [ lhs_expr; Key2 (Ident rhs_name) ] ->
      let state, lhs_ref =
        match interpret_expression ctx state lhs_expr with
        | state, _, Concrete (ConcreteReference ref) -> (state, ref)
        | _ ->
            failwith "property access where left value is not ConcreteReference"
      in
      let value =
        match List.nth state.heap lhs_ref with
        | ObjectTable scope -> StringMap.find_opt rhs_name scope
        | UnknownTable -> None
        | _ ->
            failwith
              "property access where left value references something that's \
               not an ObjectTable"
      in
      let value = Option.value value ~default:(Concrete ConcreteNil) in
      (state, Some (ObjectTableElement (lhs_ref, rhs_name)), value)
  | Unop (op, value) ->
      let state, value = interpret_rhs_expression ctx state value in
      (state, None, interpret_unop state op value)
  | Binop (op, left, right) ->
      let state, left = interpret_rhs_expression ctx state left in
      let state, value = interpret_binop_maybe_short ctx state op left right in
      (state, None, value)
  | _ ->
      Lua_parser.Pp_ast.pp_ast_show expr;
      failwith "unsupported expression"

and interpret_rhs_expression ctx state expr =
  let state, _, value = interpret_expression ctx state expr in
  (state, value)

(* TODO WARNING some of these unop handlers probably have mistakes*)
and interpret_unop (state : state) (op : string) (v : any_value) : any_value =
  match (String.trim op, v) with
  | "-", Concrete (ConcreteNumber v) -> Concrete (ConcreteNumber (Int32.neg v))
  | "not", Concrete (ConcreteBoolean b) -> Concrete (ConcreteBoolean (not b))
  | _ -> failwith (Printf.sprintf "unsupported op: %s %s" op (show_any_value v))

and interpret_binop_maybe_short (ctx : interpreter_context) (state : state)
    (op : string) (left : any_value) (right : ast) : state * any_value =
  match (op, left) with
  | "and", Concrete (ConcreteBoolean true) ->
      interpret_rhs_expression ctx state right
  | "and", Concrete (ConcreteBoolean false) ->
      (state, Concrete (ConcreteBoolean false))
  | "or", Concrete (ConcreteBoolean true) ->
      (state, Concrete (ConcreteBoolean true))
  | "or", Concrete (ConcreteBoolean false) ->
      interpret_rhs_expression ctx state right
  | _ ->
      let state, right = interpret_rhs_expression ctx state right in
      (state, interpret_binop_not_short state op left right)

(* TODO WARNING some of these binop handlers probably have mistakes*)
and interpret_binop_not_short (state : state) (op : string) (left : any_value)
    (right : any_value) : any_value =
  match (op, left, right) with
  | "+", Concrete (ConcreteNumber left), Concrete (ConcreteNumber right) ->
      Concrete (ConcreteNumber (Int32.add left right))
  | ( "+",
      Concrete (ConcreteNumber left),
      Abstract (AbstractNumberRange (right_min, right_max)) ) ->
      Abstract
        (AbstractNumberRange (Int32.add left right_min, Int32.add left right_max))
  | "-", Concrete (ConcreteNumber left), Concrete (ConcreteNumber right) ->
      Concrete (ConcreteNumber (Int32.sub left right))
  | ( "/",
      Abstract (AbstractNumberRange (left_min, left_max)),
      Concrete (ConcreteNumber right) ) ->
      Abstract
        (AbstractNumberRange (Int32.div left_min right, Int32.add left_max right))
  | "*", Concrete (ConcreteNumber left), Concrete (ConcreteNumber right) ->
      Concrete (ConcreteNumber (Int32.mul left right))
  | "%", Concrete (ConcreteNumber left), Concrete (ConcreteNumber right) ->
      let left = int_of_pico_number left in
      assert (left >= 0);
      let right = int_of_pico_number right in
      assert (right > 0);
      Concrete (ConcreteNumber (pico_number_of_int (left mod right)))
  | "==", Concrete left, Concrete right ->
      Concrete (ConcreteBoolean (equal_concrete_value left right))
  | "~=", Concrete left, Concrete right ->
      Concrete (ConcreteBoolean (not @@ equal_concrete_value left right))
  | _ ->
      failwith
        (Printf.sprintf "unsupported op: %s %s %s" (show_any_value left) op
           (show_any_value right))

and interpret_statement (ctx : interpreter_context) (state : state) (stmt : ast)
    : state =
  ctx.on_statement stmt;
  assert (state.return = None);
  match stmt with
  | Assign (Elist [ lhs_expr ], Elist [ expr ]) ->
      let state, lhs =
        match interpret_expression ctx state lhs_expr with
        | state, Some lhs, _ -> (state, lhs)
        | _, None, _ -> failwith "assignment to non-lhs expression"
      in
      let state, value = interpret_rhs_expression ctx state expr in
      let update_array_table lhs_index = function
        | ArrayTable values ->
            ArrayTable (map_ith lhs_index (fun _ -> value) values)
        | _ ->
            failwith
              "lhs of assignment references something that's not an ArrayTable"
      in
      let update_object_table lhs_name = function
        | ObjectTable o -> ObjectTable (StringMap.add lhs_name value o)
        | UnknownTable -> ObjectTable (StringMap.singleton lhs_name value)
        | _ ->
            failwith
              "lhs of assignment references something that's not an \
               ObjectTable or UnknownTable"
      in
      let lhs_ref, update_table =
        match lhs with
        | ArrayTableElement (lhs_ref, lhs_index) ->
            (lhs_ref, update_array_table lhs_index)
        | ObjectTableElement (lhs_ref, lhs_name) ->
            (lhs_ref, update_object_table lhs_name)
      in
      { state with heap = map_ith lhs_ref update_table state.heap }
  | Lassign (Elist [ Ident name ], expr) ->
      let state =
        interpret_statement ctx state (Lnames (Elist [ Ident name ]))
      in
      let state =
        interpret_statement ctx state (Assign (Elist [ Ident name ], expr))
      in
      state
  | Lnames (Elist [ Ident name ]) ->
      {
        state with
        scopes = add_local name (List.hd state.scopes) :: List.tl state.scopes;
      }
  | Function (FNlist [ Ident name ], f) ->
      interpret_statement ctx state
        (Assign (Elist [ Ident name ], Elist [ FunctionE f ]))
  | Clist [ callee_expr; Args (Elist arg_exprs) ] ->
      let state, callee_value =
        interpret_rhs_expression ctx state callee_expr
      in
      let state, arg_values =
        List.fold_left_map (interpret_rhs_expression ctx) state arg_exprs
      in
      let state, _ = interpret_call ctx state callee_value arg_values in
      state
  | Return (Elist [ expr ]) ->
      assert (is_in_call state);
      let state, value = interpret_rhs_expression ctx state expr in
      { state with return = Some (Some value) }
  | Return (Elist []) ->
      assert (is_in_call state);
      { state with return = Some None }
  | For1 (Ident i_name, i_from, i_to, Slist body) ->
      let state, i_from = interpret_rhs_expression ctx state i_from in
      let i_from = int_of_any_value i_from in
      let state, i_to = interpret_rhs_expression ctx state i_to in
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
          let state = List.fold_left (interpret_statement ctx) state body in
          { state with scopes = old_scopes })
        state i_values
  | If1 (cond, body) ->
      interpret_statement ctx state (If2 (cond, body, Slist []))
  | If2 (cond, then_body, else_body) ->
      interpret_statement ctx state
        (If3 (cond, then_body, Slist [ Elseif (Bool "true", else_body) ]))
  | If3 (first_cond, first_body, Slist elseifs) ->
      let branches =
        List.map
          (function
            | Elseif (cond, Slist body) -> (cond, body)
            | _ -> failwith "expected Elseif")
          (Elseif (first_cond, first_body) :: elseifs)
      in
      let interpret_condition state (cond, _) : state * (state * bool) =
        let sate, value = interpret_rhs_expression ctx state cond in
        let value =
          match value with
          | Concrete (ConcreteBoolean b) -> b
          | _ -> failwith "if branch condition is not a ConcreteBoolean"
        in
        (state, (state, value))
      in
      let _, matches = List.fold_left_map interpret_condition state branches in
      let state, _, body =
        List.map2
          (fun (state, did_match) (_, body) -> (state, did_match, body))
          matches branches
        |> List.find (fun (_, did_match, _) -> did_match)
      in
      let old_scopes = state.scopes in
      let state, scope_ref = allocate_raw (ObjectTable StringMap.empty) state in
      let state =
        { state with scopes = (scope_ref, StringSet.empty) :: state.scopes }
      in
      let state = List.fold_left (interpret_statement ctx) state body in
      { state with scopes = old_scopes }
  | If4 (first_cond, first_body, Slist elseifs, else_body) ->
      interpret_statement ctx state
        (If3
           ( first_cond,
             first_body,
             Slist (elseifs @ [ Elseif (Bool "true", else_body) ]) ))
  | _ ->
      Lua_parser.Pp_ast.pp_ast_show stmt;
      failwith "unsupported statement"

and interpret_call (ctx : interpreter_context) (state : state)
    (callee : any_value) (args : any_value list) : state * any_value option =
  assert (state.return = None);
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
    | None -> interpret_statement ctx state stmt
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
    | Builtin f -> f ctx state args
    | _ -> failwith "callee is not a function"
  in
  ( { state with scopes = old_scopes; return = None },
    Option.value state.return ~default:None )

let rec gc_state state : state =
  assert (List.length state.scopes = 0);
  assert (state.return = None);
  let new_refs_by_old_ref = Array.make (List.length state.heap) None in
  let heap_array = Array.of_list state.heap in
  let old_refs = Stack.create () in
  let rec f_once (old_ref : int) : int =
    let new_ref, first_visit =
      match Array.get new_refs_by_old_ref old_ref with
      | Some new_ref -> (new_ref, false)
      | None -> (Stack.length old_refs, true)
    in
    if first_visit then (
      Array.set new_refs_by_old_ref old_ref (Some new_ref);
      Stack.push old_ref old_refs;
      Array.get heap_array old_ref |> map_references_heap_value f_once |> ignore);
    new_ref
  in
  ignore @@ f_once 0;
  {
    state with
    heap =
      old_refs |> Stack.to_seq |> List.of_seq |> List.rev
      |> List.mapi (fun new_ref old_ref ->
             assert (old_ref = 0 = (new_ref = 0));
             Array.get heap_array old_ref |> map_references_heap_value f_once);
  }

and map_references_heap_value f v : heap_value =
  match v with
  | ArrayTable values ->
      ArrayTable (List.map (map_references_any_value f) values)
  | ObjectTable values ->
      ObjectTable (StringMap.map (map_references_any_value f) values)
  | UnknownTable -> v
  | Function (args, body, scopes) ->
      Function (args, body, List.map (fun (ref, names) -> (f ref, names)) scopes)
  | Builtin _ -> v

and map_references_concrete_value f v : concrete_value =
  match v with
  | ConcreteNumber _ -> v
  | ConcreteBoolean _ -> v
  | ConcreteReference old_ref -> ConcreteReference (f old_ref)
  | ConcreteNil -> v

and map_references_abstract_value f v : abstract_value =
  match v with
  | AbstractOneOf values ->
      AbstractOneOf (List.map (map_references_concrete_value f) values)
  | AbstractNumberRange _ -> v

and map_references_any_value f v : any_value =
  match v with
  | Concrete v -> Concrete (map_references_concrete_value f v)
  | Abstract v -> Abstract (map_references_abstract_value f v)

let show_heap_value_short v =
  match v with
  | ArrayTable values ->
      Printf.sprintf "ArrayTable (%d elements)" (List.length values)
  | ObjectTable values ->
      Printf.sprintf "ObjectTable (%d elements)" (StringMap.cardinal values)
  | UnknownTable -> "UnknownTable"
  | Function (args, _, scopes) ->
      Printf.sprintf "Function (%d args, %d scopes)" (List.length args)
        (List.length scopes)
  | Builtin _ -> "Builtin"

let print_heap_short (heap : heap_value list) : unit =
  List.iteri
    (fun i v -> Printf.printf "%d: %s\n" i (show_heap_value_short v))
    heap

let interpret_program (state : state) (program : ast) : state =
  let program =
    match program with
    | Slist program -> program
    | _ -> failwith "expected SList"
  in
  let ctx = { on_statement = (fun _ -> ()) } in
  List.fold_left (interpret_statement ctx) state program

let debug_program (state : state) (program : ast) : state =
  let program =
    match program with
    | Slist program -> program
    | _ -> failwith "expected SList"
  in
  let ctx =
    {
      on_statement =
        (fun stmt ->
          Lua_parser.Pp_lua.pp_lua stmt;
          print_endline "");
    }
  in
  List.fold_left (interpret_statement ctx) state program

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

let () = assert (List.length map_data = 8192)

let return_from_builtin (value : any_value) (state : state) : state =
  assert (state.return = None);
  { state with return = Some (Some value) }

let builtin_print _ state args =
  List.iter (fun v -> print_endline (show_any_value v)) args;
  state

let builtin_add _ state args =
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

let builtin_rnd _ state args =
  let max =
    match args with
    | [ Concrete (ConcreteNumber max) ] -> max
    | _ -> failwith "bad arguments to rnd"
  in
  assert (max >= pico_number_of_int 0);
  return_from_builtin
    (Abstract (AbstractNumberRange (pico_number_of_int 0, max)))
    state

let builtin_flr _ state args =
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

let builtin_foreach ctx state args =
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
    let state, _ = interpret_call ctx state cb [ value ] in
    state
  in
  List.fold_left call_iteration state values

let builtin_mget _ state args =
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

let builtin_min _ state args =
  let a, b =
    match args with
    | [ Concrete (ConcreteNumber a); Concrete (ConcreteNumber b) ] -> (a, b)
    | _ -> failwith "bad arguments to min"
  in
  return_from_builtin (Concrete (ConcreteNumber (Int32.min a b))) state

let builtin_max _ state args =
  let a, b =
    match args with
    | [ Concrete (ConcreteNumber a); Concrete (ConcreteNumber b) ] -> (a, b)
    | _ -> failwith "bad arguments to max"
  in
  return_from_builtin (Concrete (ConcreteNumber (Int32.max a b))) state

let builtin_dead _ state args = state

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
      [
        ("print", builtin_print);
        ("add", builtin_add);
        ("rnd", builtin_rnd);
        ("flr", builtin_flr);
        ("foreach", builtin_foreach);
        ("mget", builtin_mget);
        ("min", builtin_min);
        ("max", builtin_max);
        ("music", builtin_dead);
        ("sfx", builtin_dead);
      ]
  in
  state

let () =
  let state = initial_state in
  let state = interpret_program state example_program in
  let state = gc_state state in
  let state =
    "_init()" |> Lua_parser.Parse.parse_from_string |> interpret_program state
  in
  let old_heap_size = List.length state.heap in
  let state = gc_state state in
  print_heap_short state.heap;
  Printf.printf "heap size before gc: %d\n" old_heap_size;
  Printf.printf "heap size after gc: %d\n" (List.length state.heap)
