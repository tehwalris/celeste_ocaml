open Lua_parser.Ast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(* TODO WARNING some of these pico_number functions probably have mistakes*)

type pico_number = Int32.t [@@deriving show]

let whole_int_of_pico_number (n : pico_number) : int =
  Int32.shift_right n 16 |> Int32.to_int

let fraction_int_of_pico_number (n : pico_number) : int =
  Int32.shift_right_logical (Int32.shift_left n 16) 16 |> Int32.to_int

let float_of_pico_number (n : pico_number) : float =
  let whole = Float.of_int @@ whole_int_of_pico_number n in
  let fraction = Float.of_int @@ fraction_int_of_pico_number n in
  whole +. (fraction /. 65536.)

let pp_pico_number (f : Format.formatter) (n : pico_number) =
  Format.fprintf f "(pico_number %d + (%d / 65536))"
    (whole_int_of_pico_number n)
    (fraction_int_of_pico_number n)

let pico_number_of_ints (whole_n : int) (fraction_n : int) : pico_number =
  assert (whole_n >= -32768 && whole_n < 32768);
  assert (fraction_n >= 0 && fraction_n < 65536);
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
  if n < 0. then Int32.neg @@ pico_number_of_float (-.n)
  else
    pico_number_of_ints (int_of_float n)
      (int_of_float ((n -. floor n) *. 65536.))

let pico_number_of_string n = n |> float_of_string |> pico_number_of_float

let int_of_pico_number (n : pico_number) : int =
  assert (fraction_int_of_pico_number n = 0);
  whole_int_of_pico_number n

let equal_pico_number = Int32.equal

let pico_number_mul (a : pico_number) (b : pico_number) =
  let result_high = Int64.mul (Int64.of_int32 a) (Int64.of_int32 b) in
  let result_low = Int64.shift_right result_high 16 in
  Int64.to_int32 result_low

let pico_number_div (a : pico_number) (b : pico_number) =
  let a_high = Int64.shift_left (Int64.of_int32 a) 16 in
  let result = Int64.div a_high (Int64.of_int32 b) in
  Int64.to_int32 result

type concrete_value =
  | ConcreteNumber of pico_number
  | ConcreteBoolean of bool
  | ConcreteReference of int
  | ConcreteNil
  | ConcreteString of string
[@@deriving show, eq]

type abstract_value =
  | AbstractOneOf of concrete_value list
  | AbstractNumberRange of pico_number * pico_number
  | AbstractString
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

type interpreter_context = { on_statement : ast -> unit; print : bool }
type identifier = Identifier of string [@@deriving show]

type heap_value =
  | ArrayTable of array_table
  | ObjectTable of object_table
  | UnknownTable
  | Function of string list * ast list * scope list
  | Builtin of (interpreter_context -> state -> any_value list -> state list)
[@@deriving show]

and state = {
  heap : heap_value list;
  scopes : scope list;
  return : any_value option option;
  break : bool;
}
[@@deriving show]

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

let is_skipping (state : state) : bool =
  Option.is_some state.return || state.break

let pad_or_drop p n l =
  if List.length l > n then BatList.take n l
  else l @ List.init (n - List.length l) (fun _ -> p)

let concrete_number_of_any_value v =
  match v with
  | Concrete (ConcreteNumber n) -> n
  | _ -> failwith "not a concrete number"

let concat_fold_left f init list =
  List.fold_left (fun xs y -> List.concat_map (fun x -> f x y) xs) init list

let concat_fold_left_map (f : 'a -> 'b -> ('a * 'c) list) (init : 'a list)
    (list : 'b list) : ('a * 'c list) list =
  list
  |> List.fold_left
       (fun (xs : ('a * 'c list) list) (y : 'b) ->
         xs
         |> List.concat_map (fun (old_acc, old_map_values) ->
                f old_acc y
                |> List.map (fun (acc, map_value) ->
                       (acc, map_value :: old_map_values))))
       (List.map (fun init -> (init, [])) init)
  |> List.map (fun (acc, map_values) -> (acc, List.rev map_values))

let only xs =
  match xs with
  | [ x ] -> x
  | _ -> failwith "list does not contain exactly one element"

let rec bools_of_any_value (v : any_value) : (bool * any_value) list =
  match v with
  | Concrete (ConcreteBoolean b) -> [ (b, v) ]
  | Concrete (ConcreteNumber _) -> [ (true, v) ]
  | Concrete ConcreteNil -> [ (false, v) ]
  | Abstract (AbstractNumberRange _) -> [ (true, v) ]
  | Abstract (AbstractOneOf options) ->
      List.concat_map (fun o -> bools_of_any_value (Concrete o)) options
  | Abstract AbstractString -> [ (true, v) ]
  | _ ->
      failwith
        "bools_from_any_value called on a value which could not be converted \
         to a boolean"

let rec any_value_of_bools_direct (maybe_false : bool) (maybe_true : bool) :
    any_value =
  match (maybe_false, maybe_true) with
  | false, false -> Abstract (AbstractOneOf [])
  | false, true -> Concrete (ConcreteBoolean true)
  | true, false -> Concrete (ConcreteBoolean false)
  | true, true ->
      Abstract (AbstractOneOf [ ConcreteBoolean false; ConcreteBoolean true ])

let rec any_value_of_bools (bools : bool list) : any_value =
  let maybe_false, maybe_true =
    List.fold_left
      (fun (maybe_false, maybe_true) b ->
        (maybe_false || not b, maybe_true || b))
      (false, false) bools
  in
  any_value_of_bools_direct maybe_false maybe_true

let rec number_range_of_any_value (v : any_value) : pico_number * pico_number =
  match v with
  | Concrete (ConcreteNumber v) -> (v, v)
  | Abstract (AbstractNumberRange (v_min, v_max)) ->
      assert (v_min <= v_max);
      (v_min, v_max)
  | _ ->
      failwith
        "number_range_from_any_value called on a value which could not be \
         converted to a number range"

let rec any_value_of_number_range (v_min : pico_number) (v_max : pico_number) :
    any_value =
  assert (v_min <= v_max);
  if v_min == v_max then Concrete (ConcreteNumber v_min)
  else Abstract (AbstractNumberRange (v_min, v_max))

let rec interpret_expression (ctx : interpreter_context) (state : state)
    (expr : ast) : (state * lhs_value option * any_value) list =
  match expr with
  | Number n ->
      [ (state, None, Concrete (ConcreteNumber (pico_number_of_string n))) ]
  | Bool "true" -> [ (state, None, Concrete (ConcreteBoolean true)) ]
  | Bool "false" -> [ (state, None, Concrete (ConcreteBoolean false)) ]
  | Bool "nil" -> [ (state, None, Concrete ConcreteNil) ]
  | String s -> [ (state, None, Concrete (ConcreteString s)) ]
  | Ident name ->
      let lhs_ref, lhs_name, value = get_by_scope name state in
      [ (state, Some (ObjectTableElement (lhs_ref, lhs_name)), value) ]
  | Table (Elist []) ->
      let state, value = allocate UnknownTable state in
      [ (state, None, value) ]
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
      let initializations =
        concat_fold_left_map
          (fun state (_, expr) -> interpret_rhs_expression ctx state expr)
          [ state ] initializers
      in
      initializations
      |> List.map (fun (state, initializer_values) ->
             let value_map =
               List.map2
                 (fun (name, _) value -> (name, value))
                 initializers initializer_values
               |> List.to_seq |> StringMap.of_seq
             in
             let state, value = allocate (ObjectTable value_map) state in
             (state, None, value))
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
      [ (state, None, value) ]
  | Clist [ callee_expr; Args (Elist arg_exprs) ] ->
      let state, callee_value =
        only @@ interpret_rhs_expression ctx state callee_expr
      in
      let state, arg_values =
        List.fold_left_map
          (fun state expr -> only @@ interpret_rhs_expression ctx state expr)
          state arg_exprs
      in
      interpret_call ctx state callee_value arg_values
      |> List.map (fun (state, return_value) ->
             (state, None, Option.get return_value))
  | Clist [ lhs_expr; Key1 rhs_expr ] ->
      let state, lhs_ref =
        match interpret_expression ctx state lhs_expr with
        | [ (state, _, Concrete (ConcreteReference ref)) ] -> (state, ref)
        | _ ->
            failwith "element access where left value is not ConcreteReference"
      in
      let state, rhs_index =
        match interpret_expression ctx state rhs_expr with
        | [ (state, _, Concrete (ConcreteNumber i)) ] ->
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
      [ (state, Some (ArrayTableElement (lhs_ref, rhs_index)), value) ]
  | Clist [ lhs_expr; Key2 (Ident rhs_name) ] ->
      let state, lhs_ref =
        match interpret_expression ctx state lhs_expr with
        | [ (state, _, Concrete (ConcreteReference ref)) ] -> (state, ref)
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
      [ (state, Some (ObjectTableElement (lhs_ref, rhs_name)), value) ]
  | Unop (op, value) ->
      let state, value = only @@ interpret_rhs_expression ctx state value in
      [ (state, None, interpret_unop state op value) ]
  | Binop (op, left, right) ->
      interpret_rhs_expression ctx state left
      |> List.concat_map (fun (state, left) ->
             interpret_binop_maybe_short ctx state op left right
             |> List.map (fun (state, value) -> (state, None, value)))
  | Pexp expr -> interpret_expression ctx state expr
  | _ ->
      Lua_parser.Pp_ast.pp_ast_show expr;
      failwith "unsupported expression"

and interpret_rhs_expression ctx state expr =
  interpret_expression ctx state expr
  |> List.map (fun (state, _, value) -> (state, value))

(* TODO WARNING some of these unop handlers probably have mistakes*)
and interpret_unop (state : state) (op : string) (v : any_value) : any_value =
  match (String.trim op, v) with
  | "-", Concrete (ConcreteNumber v) -> Concrete (ConcreteNumber (Int32.neg v))
  | "not", Concrete (ConcreteBoolean b) -> Concrete (ConcreteBoolean (not b))
  | _ -> failwith (Printf.sprintf "unsupported op: %s %s" op (show_any_value v))

and interpret_binop_maybe_short (ctx : interpreter_context) (state : state)
    (op : string) (left : any_value) (right : ast) : (state * any_value) list =
  match (op, left) with
  | "and", _ ->
      bools_of_any_value left
      |> List.concat_map (fun (left_bool, left) ->
             match left_bool with
             | true -> interpret_rhs_expression ctx state right
             | false -> [ (state, left) ])
  | "or", _ ->
      bools_of_any_value left
      |> List.concat_map (fun (left_bool, left) ->
             match left_bool with
             | true -> [ (state, left) ]
             | false -> interpret_rhs_expression ctx state right)
  | op, Abstract (AbstractOneOf left_options) ->
      left_options
      |> List.map (fun v -> Concrete v)
      |> List.concat_map (fun left ->
             interpret_binop_maybe_short ctx state op left right)
  | _ ->
      interpret_rhs_expression ctx state right
      |> List.map (fun (state, right) ->
             (state, interpret_binop_not_short state op left right))

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
  | ( "+",
      Abstract (AbstractNumberRange (left_min, left_max)),
      Abstract (AbstractNumberRange (right_min, right_max)) ) ->
      Abstract
        (AbstractNumberRange
           (Int32.add left_min right_min, Int32.add left_max right_max))
  | "-", Concrete (ConcreteNumber left), Concrete (ConcreteNumber right) ->
      Concrete (ConcreteNumber (Int32.sub left right))
  | "/", Concrete (ConcreteNumber left), Concrete (ConcreteNumber right) ->
      Concrete (ConcreteNumber (pico_number_div left right))
  | ( "/",
      Abstract (AbstractNumberRange (left_min, left_max)),
      Concrete (ConcreteNumber right) ) ->
      assert (Int32.compare right (pico_number_of_int 0) > 0);
      Abstract
        (AbstractNumberRange
           (pico_number_div left_min right, pico_number_div left_max right))
  | "*", Concrete (ConcreteNumber left), Concrete (ConcreteNumber right) ->
      Concrete (ConcreteNumber (pico_number_mul left right))
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
  | "<", _, _ | "<=", _, _ | ">", _, _ | ">=", _, _ ->
      let compare_concrete left right =
        (match op with
        | "<" -> ( < )
        | "<=" -> ( <= )
        | ">" -> ( > )
        | ">=" -> ( >= )
        | _ -> failwith "unreachable")
          (Int32.compare left right) 0
      in
      let compare_range (left_min, left_max) (right_min, right_max) =
        any_value_of_bools
          [
            compare_concrete left_min right_min;
            compare_concrete left_min right_max;
            compare_concrete left_max right_min;
            compare_concrete left_max right_max;
          ]
      in
      compare_range
        (number_range_of_any_value left)
        (number_range_of_any_value right)
  | "..", _, _ -> Abstract AbstractString
  | _ ->
      failwith
        (Printf.sprintf "unsupported op: %s %s %s" (show_any_value left) op
           (show_any_value right))

and interpret_statement (ctx : interpreter_context) (state : state) (stmt : ast)
    : state list =
  if not (is_skipping state) then ctx.on_statement stmt;
  match (is_skipping state, stmt) with
  | true, _ -> [ state ]
  | _, Assign (Elist [ lhs_expr ], Elist [ expr ]) ->
      let state, lhs =
        match only @@ interpret_expression ctx state lhs_expr with
        | state, Some lhs, _ -> (state, lhs)
        | _, None, _ -> failwith "assignment to non-lhs expression"
      in
      interpret_rhs_expression ctx state expr
      |> List.map (fun (state, value) ->
             let update_array_table lhs_index = function
               | ArrayTable values ->
                   ArrayTable (map_ith lhs_index (fun _ -> value) values)
               | _ ->
                   failwith
                     "lhs of assignment references something that's not an \
                      ArrayTable"
             in
             let update_object_table lhs_name = function
               | ObjectTable o -> ObjectTable (StringMap.add lhs_name value o)
               | UnknownTable ->
                   ObjectTable (StringMap.singleton lhs_name value)
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
             { state with heap = map_ith lhs_ref update_table state.heap })
  | _, Lassign (Elist [ Ident name ], expr) ->
      let state =
        only @@ interpret_statement ctx state (Lnames (Elist [ Ident name ]))
      in
      let state =
        interpret_statement ctx state (Assign (Elist [ Ident name ], expr))
      in
      state
  | _, Lnames (Elist [ Ident name ]) ->
      [
        {
          state with
          scopes = add_local name (List.hd state.scopes) :: List.tl state.scopes;
        };
      ]
  | _, Function (FNlist [ Ident name ], f) ->
      interpret_statement ctx state
        (Assign (Elist [ Ident name ], Elist [ FunctionE f ]))
  | _, Clist [ callee_expr; Args (Elist arg_exprs) ] ->
      let state, callee_value =
        only @@ interpret_rhs_expression ctx state callee_expr
      in
      let state, arg_values =
        List.fold_left_map
          (fun state expr -> only @@ interpret_rhs_expression ctx state expr)
          state arg_exprs
      in
      interpret_call ctx state callee_value arg_values
      |> List.map (fun (state, _) -> state)
  | _, Return (Elist [ expr ]) ->
      assert (is_in_call state);
      let state, value = only @@ interpret_rhs_expression ctx state expr in
      [ { state with return = Some (Some value) } ]
  | _, Return (Elist []) ->
      assert (is_in_call state);
      [ { state with return = Some None } ]
  | _, Break -> [ { state with break = true } ]
  | _, For1 (Ident i_name, i_from, i_to, Slist body) ->
      let state, i_from = only @@ interpret_rhs_expression ctx state i_from in
      let i_from = concrete_number_of_any_value i_from in
      let state, i_to = only @@ interpret_rhs_expression ctx state i_to in
      let i_to = concrete_number_of_any_value i_to in
      assert (i_from <= i_to);
      let i_values =
        BatEnum.unfold i_from (fun i ->
            if i <= i_to then
              Some
                (Concrete (ConcreteNumber i), Int32.add i (pico_number_of_int 1))
            else None)
        |> BatList.of_enum
      in
      let state =
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
            let state =
              List.fold_left
                (fun state ast -> only @@ interpret_statement ctx state ast)
                state body
            in
            { state with scopes = old_scopes; break = false })
          state i_values
      in
      [ state ]
  | _, If1 (cond, body) ->
      interpret_statement ctx state (If2 (cond, body, Slist []))
  | _, If2 (cond, then_body, else_body) ->
      interpret_statement ctx state
        (If3 (cond, then_body, Slist [ Elseif (Bool "true", else_body) ]))
  | _, If3 (first_cond, first_body, Slist elseifs) ->
      let branches =
        List.map
          (function
            | Elseif (cond, Slist body) -> (cond, body)
            | _ -> failwith "expected Elseif")
          (Elseif (first_cond, first_body)
          :: (elseifs @ [ Elseif (Bool "true", Slist []) ]))
      in
      let interpret_condition (state, already_matched) (cond, _) :
          (state * bool) list =
        if already_matched then [ (state, true) ]
        else
          interpret_rhs_expression ctx state cond
          |> List.concat_map (fun (state, value) ->
                 bools_of_any_value value
                 |> List.map (fun (value, _) -> (state, value)))
      in
      concat_fold_left_map
        (fun acc cond ->
          interpret_condition acc cond |> List.map (fun v -> (v, v)))
        [ (state, false) ]
        branches
      |> List.concat_map (fun (_, matches) ->
             let state, _, body =
               List.map2
                 (fun (state, did_match) (_, body) -> (state, did_match, body))
                 matches branches
               |> List.find (fun (_, did_match, _) -> did_match)
             in
             let old_scopes = state.scopes in
             let state, scope_ref =
               allocate_raw (ObjectTable StringMap.empty) state
             in
             let state =
               {
                 state with
                 scopes = (scope_ref, StringSet.empty) :: state.scopes;
               }
             in
             concat_fold_left
               (fun state ast -> interpret_statement ctx state ast)
               [ state ] body
             |> List.map (fun state -> { state with scopes = old_scopes }))
  | _, If4 (first_cond, first_body, Slist elseifs, else_body) ->
      interpret_statement ctx state
        (If3
           ( first_cond,
             first_body,
             Slist (elseifs @ [ Elseif (Bool "true", else_body) ]) ))
  | _, _ ->
      Lua_parser.Pp_ast.pp_ast_show stmt;
      failwith "unsupported statement"

and interpret_call (ctx : interpreter_context) (state : state)
    (callee : any_value) (args : any_value list) :
    (state * any_value option) list =
  assert (not (is_skipping state));
  let ref =
    match callee with
    | Concrete (ConcreteReference ref) -> ref
    | _ -> failwith "callee is not a concrete reference"
  in
  let old_scopes = state.scopes in
  let state, scope_ref = allocate_raw (ObjectTable StringMap.empty) state in
  let states =
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
        concat_fold_left
          (fun state ast -> interpret_statement ctx state ast)
          [ state ] body
    | Builtin f -> f ctx state args
    | _ -> failwith "callee is not a function"
  in
  List.map
    (fun state ->
      ( { state with scopes = old_scopes; return = None },
        Option.value state.return ~default:None ))
    states

let rec gc_state state : state =
  assert (List.length state.scopes = 0);
  assert (not (is_skipping state));
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
  | ConcreteString _ -> v

and map_references_abstract_value f v : abstract_value =
  match v with
  | AbstractOneOf values ->
      AbstractOneOf (List.map (map_references_concrete_value f) values)
  | AbstractNumberRange _ -> v
  | AbstractString -> v

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

let interpret_program (state : state) (program : ast) : state list =
  let program =
    match program with
    | Slist program -> program
    | _ -> failwith "expected SList"
  in
  let ctx = { on_statement = (fun _ -> ()); print = false } in
  concat_fold_left (interpret_statement ctx) [ state ] program

let debug_program (state : state) (program : ast) : state list =
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
      print = true;
    }
  in
  concat_fold_left (interpret_statement ctx) [ state ] program

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

let load_hex_file name size =
  let data =
    BatFile.with_file_in name BatIO.read_all
    |> Str.global_replace (Str.regexp "[^a-f0-9]") ""
    |> String.to_seq |> List.of_seq |> parse_hex_bytes |> Array.of_list
  in
  assert (Array.length data = size);
  data

let map_data = load_hex_file "map-data.txt" 8192
let flag_data = load_hex_file "flag-data.txt" 256

let return_from_builtin (value : any_value) (state : state) : state =
  assert (not (is_skipping state));
  { state with return = Some (Some value) }

let builtin_print ctx state args =
  if ctx.print then List.iter (fun v -> print_endline (show_any_value v)) args;
  [ state ]

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
  [ { state with heap = map_ith target_ref update_table state.heap } ]

let builtin_rnd _ state args =
  let max =
    match args with
    | [ Concrete (ConcreteNumber max) ] -> max
    | _ -> failwith "bad arguments to rnd"
  in
  assert (max >= pico_number_of_int 0);
  [
    return_from_builtin
      (Abstract (AbstractNumberRange (pico_number_of_int 0, max)))
      state;
  ]

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
  [ return_from_builtin result state ]

let builtin_foreach ctx state args =
  let table_ref, cb =
    match args with
    | [ Concrete (ConcreteReference target_ref); cb ] -> (target_ref, cb)
    | _ -> failwith "bad arguments to foreach"
  in
  let values =
    match List.nth state.heap table_ref with
    | ArrayTable values -> values
    | UnknownTable -> []
    | _ -> failwith "expected ArrayTable or UnknownTable"
  in
  let call_iteration state value =
    interpret_call ctx state cb [ value ] |> List.map (fun (state, _) -> state)
  in
  concat_fold_left call_iteration [ state ] values

let builtin_mget _ state args =
  let x, y =
    match args with
    | [ Concrete (ConcreteNumber x); Concrete (ConcreteNumber y) ] -> (x, y)
    | _ -> failwith "bad arguments to rnd"
  in
  let x = int_of_pico_number x in
  assert (x >= 0 && x < 128);
  let y = int_of_pico_number y in
  assert (y >= 0 && y < 64);
  let v = Array.get map_data (x + (y * 128)) in
  [
    return_from_builtin (Concrete (ConcreteNumber (pico_number_of_int v))) state;
  ]

let builtin_fget _ state args =
  let i, b =
    match args with
    | [ Concrete (ConcreteNumber i); Concrete (ConcreteNumber b) ] -> (i, b)
    | _ -> failwith "bad arguments to rnd"
  in
  let i = int_of_pico_number i in
  let b = int_of_pico_number b in
  assert (b >= 0 && b < 8);
  let v = Array.get flag_data i in
  [
    return_from_builtin
      (Concrete (ConcreteBoolean (Int.logand v (Int.shift_left 1 b) != 0)))
      state;
  ]

let builtin_minmax minmax _ state args =
  let (a_min, a_max), (b_min, b_max) =
    match args with
    | [ a; b ] -> (number_range_of_any_value a, number_range_of_any_value b)
    | _ -> failwith "bad arguments to min/max"
  in
  [
    return_from_builtin
      (any_value_of_number_range (minmax a_min b_min) (minmax a_max b_max))
      state;
  ]

let builtin_abs _ state args =
  let abs_pico (n : pico_number) : pico_number = Int32.abs n in
  let result =
    match args with
    | [ Concrete (ConcreteNumber v) ] -> Concrete (ConcreteNumber (abs_pico v))
    | _ -> failwith "bad arguments to abs"
  in
  [ return_from_builtin result state ]

let builtin_count ctx state args =
  let table_ref =
    match args with
    | [ Concrete (ConcreteReference target_ref) ] -> target_ref
    | _ -> failwith "bad arguments to count"
  in
  let values =
    match List.nth state.heap table_ref with
    | ArrayTable values -> values
    | UnknownTable -> []
    | _ -> failwith "expected ArrayTable or UnknownTable"
  in
  [
    return_from_builtin
      (Concrete (ConcreteNumber (pico_number_of_int @@ List.length values)))
      state;
  ]

let builtin_btn ctx state args =
  let i =
    match args with
    | [ Concrete (ConcreteNumber n) ] -> int_of_pico_number n
    | _ -> failwith "bad arguments to btn"
  in
  assert (i >= 0 && i <= 5);
  [
    return_from_builtin
      (Abstract (AbstractOneOf [ ConcreteBoolean false; ConcreteBoolean true ]))
      state;
  ]

let builtin_sincos sincos _ state args =
  let sincos_pico a =
    a |> float_of_pico_number
    |> (fun a -> sincos (1. -. a) *. 2. *. Float.pi)
    |> pico_number_of_float
  in
  let result =
    match args with
    | [ Concrete (ConcreteNumber v) ] ->
        Concrete (ConcreteNumber (sincos_pico v))
    | [ Abstract (AbstractNumberRange _) ] ->
        Abstract
          (AbstractNumberRange (pico_number_of_int (-1), pico_number_of_int 1))
    | _ -> failwith "bad arguments to sin/cos"
  in
  [ return_from_builtin result state ]

let builtin_dead _ state args = [ state ]

let initial_state =
  let state =
    {
      heap = [ ObjectTable StringMap.empty ];
      scopes = [];
      return = None;
      break = false;
    }
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
        ("fget", builtin_fget);
        ("min", builtin_minmax min);
        ("max", builtin_minmax max);
        ("abs", builtin_abs);
        ("count", builtin_count);
        ("btn", builtin_btn);
        ("sin", builtin_sincos sin);
        ("cos", builtin_sincos cos);
        ("music", builtin_dead);
        ("sfx", builtin_dead);
        ("pal", builtin_dead);
        ("rectfill", builtin_dead);
        ("circfill", builtin_dead);
        ("map", builtin_dead);
        ("spr", builtin_dead);
      ]
  in
  state

let () =
  let state = initial_state in
  let state = only @@ interpret_program state example_program in
  let state = gc_state state in
  let state =
    "_init()" |> Lua_parser.Parse.parse_from_string |> interpret_program state
    |> only
  in
  let old_heap_size = List.length state.heap in
  let state = gc_state state in
  print_heap_short state.heap;
  Printf.printf "heap size before gc: %d\n" old_heap_size;
  Printf.printf "heap size after gc: %d\n" (List.length state.heap);
  let interpret_multi interpret_or_debug_program =
    List.fold_left (fun states stmt_str ->
        print_endline stmt_str;
        let states =
          states
          |> List.concat_map (fun state ->
                 stmt_str |> Lua_parser.Parse.parse_from_string
                 |> interpret_or_debug_program state)
          |> List.map gc_state
        in
        Printf.printf "possible executions: %d\n" (List.length states);
        states)
  in
  let states =
    List.init 100 (fun i ->
        [ Printf.sprintf "print(%d)" i; "_update()"; "_draw()" ])
    |> List.flatten
    |> interpret_multi interpret_program [ state ]
  in
  (* let states = interpret_multi debug_program states [ "_update()" ] in
     let states = interpret_multi debug_program states [ "_draw()" ] in *)
  ignore states