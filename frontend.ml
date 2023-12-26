open Lua_parser.Ast

let rec iterate_until_stable (f : 'a -> 'a) (x : 'a) : 'a =
  let x' = f x in
  if x' = x then x else iterate_until_stable f x'

type stream_el =
  | L of Ir.label
  | I of Ir.local_id * Ir.instruction
  | T of Ir.local_id * Ir.terminator
  | F of Ir.fun_def
[@@deriving show]

type stream = stream_el list [@@deriving show]

let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x

let unsupported_ast ast =
  Lua_parser.Pp_lua.pp_lua ast;
  Printf.printf "\n";
  Lua_parser.Pp_ast.pp_ast_show ast;
  failwith "unsupported AST"

let cfg_of_stream (code : stream) : Ir.cfg * Ir.fun_def list =
  let make_block instructions terminator : Ir.block =
    { instructions; terminator = Option.get terminator }
  in
  let unused_instructions, unused_terminator, named_blocks, fun_defs =
    List.fold_left
      (fun (unused_instructions, unused_terminator, named_blocks, fun_defs) el ->
        match el with
        | L label ->
            ( [],
              None,
              (label, make_block unused_instructions unused_terminator)
              :: named_blocks,
              fun_defs )
        | I (id, instruction) ->
            if Option.is_none unused_terminator then
              Printf.printf "%s\n%s"
                (Ir.show_instruction @@ instruction)
                (show_stream code);
            assert (Option.is_some unused_terminator);
            ( (id, instruction) :: unused_instructions,
              unused_terminator,
              named_blocks,
              fun_defs )
        | T (id, terminator) ->
            assert (unused_instructions = []);
            if Option.is_some unused_terminator then
              Printf.printf "%s\n%s"
                (Ir.show_terminator @@ snd @@ Option.get unused_terminator)
                (show_stream code);
            assert (Option.is_none unused_terminator);
            (unused_instructions, Some (id, terminator), named_blocks, fun_defs)
        | F fun_def ->
            ( unused_instructions,
              unused_terminator,
              named_blocks,
              fun_def :: fun_defs ))
      ([], None, [], []) code
  in
  ( Contract_blocks.contract_blocks
      {
        entry = make_block unused_instructions unused_terminator;
        named = named_blocks;
      },
    fun_defs )

module Ctxt = struct
  type t = (string * Ir.local_id) list

  let empty = []
  let add (c : t) (name : string) (value : Ir.local_id) : t = (name, value) :: c
  let lookup : string -> t -> Ir.local_id = List.assoc
  let lookup_opt : string -> t -> Ir.local_id option = List.assoc_opt
end

let gen_local_id : unit -> Ir.local_id =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

let gen_global_id : string -> Ir.global_id =
  let i = ref 0 in
  fun s ->
    incr i;
    Printf.sprintf "%s_%d" s !i

let gen_label : string -> Ir.label =
  let i = ref 0 in
  fun s ->
    incr i;
    Printf.sprintf "%s_%d" s !i

let gen_id_and_stream (insn : Ir.instruction) : Ir.local_id * stream =
  let id = gen_local_id () in
  (id, [ I (id, insn) ])

let add_terminator_if_needed (t : Ir.terminator) (code : stream) : stream =
  match code with T _ :: _ -> code | _ -> code >:: T (gen_local_id (), t)

let simple_single_quote_lua_string_regex =
  Re.Perl.compile_pat {|^'([^\\\n']*)'$|}

let simple_double_quote_lua_string_regex =
  Re.Perl.compile_pat {|^"([^\\\n"]*)"$|}

let parse_lua_string (s : string) : string =
  let single_quote_match = Re.exec_opt simple_single_quote_lua_string_regex s in
  let double_quote_match = Re.exec_opt simple_double_quote_lua_string_regex s in
  match (single_quote_match, double_quote_match) with
  | Some m, None -> Re.Group.get m 1
  | None, Some m -> Re.Group.get m 1
  | _ -> unsupported_ast (String s)

let rec compile_lhs_expression (c : Ctxt.t) (expr : ast)
    (create_if_missing : bool) : Ir.local_id * string option * stream =
  match expr with
  | Ident name -> (
      match Ctxt.lookup_opt name c with
      | Some id -> (id, Some name, [])
      | None ->
          let id, stream =
            gen_id_and_stream (Ir.GetGlobal (name, create_if_missing))
          in
          (id, Some name, stream))
  | Clist [ lhs_expr; Key1 rhs_expr ] ->
      let lhs_id, lhs_hint, lhs_stream =
        compile_rhs_expression c lhs_expr None
      in
      let rhs_id, _, rhs_stream = compile_rhs_expression c rhs_expr None in
      let result_id = gen_local_id () in
      ( result_id,
        lhs_hint,
        lhs_stream >@ rhs_stream
        >:: I (result_id, Ir.GetIndex (lhs_id, rhs_id, create_if_missing)) )
  | Clist [ lhs_expr; Key2 (Ident field_name) ] ->
      let lhs_id, _, lhs_stream = compile_rhs_expression c lhs_expr None in
      let result_id = gen_local_id () in
      ( result_id,
        Some field_name,
        lhs_stream
        >:: I (result_id, Ir.GetField (lhs_id, field_name, create_if_missing))
      )
  | _ -> unsupported_ast expr

and compile_rhs_expression (c : Ctxt.t) (expr : ast)
    (hint_for_function_name : string option) :
    Ir.local_id * string option * stream =
  let no_hint (id, stream) = (id, None, stream) in
  match expr with
  | Table (Elist assignments) ->
      let table_id = gen_local_id () in
      let assignment_code =
        List.concat_map
          (function
            | Assign (Ident field_name, value_expr) ->
                let field_id = gen_local_id () in
                let value_id, _, value_code =
                  compile_rhs_expression c value_expr hint_for_function_name
                in
                value_code
                >:: I (field_id, Ir.GetField (table_id, field_name, true))
                >:: I (gen_local_id (), Ir.Store (field_id, value_id))
            | _ -> failwith "only Assign is allowed in Table expression")
          (List.rev assignments)
      in
      ( table_id,
        None,
        List.rev
          [
            I (table_id, Ir.Alloc);
            I (gen_local_id (), Ir.StoreEmptyTable table_id);
          ]
        >@ assignment_code )
  | Number s ->
      no_hint @@ gen_id_and_stream (Ir.NumberConstant (Pico_number.of_string s))
  | Bool "true" -> no_hint @@ gen_id_and_stream (Ir.BoolConstant true)
  | Bool "false" -> no_hint @@ gen_id_and_stream (Ir.BoolConstant false)
  | Bool "nil" -> no_hint @@ gen_id_and_stream Ir.NilConstant
  | String s ->
      no_hint @@ gen_id_and_stream (Ir.StringConstant (parse_lua_string s))
  | Unop (op, inner_expr) ->
      let inner_id, inner_hint, inner_stream =
        compile_rhs_expression c inner_expr hint_for_function_name
      in
      let result_id, result_stream =
        gen_id_and_stream (UnaryOp (String.trim op, inner_id))
      in
      (result_id, inner_hint, inner_stream >@ result_stream)
  | Binop ("and", left_expr, right_expr) ->
      no_hint @@ compile_and_or true c left_expr right_expr
  | Binop ("or", left_expr, right_expr) ->
      no_hint @@ compile_and_or false c left_expr right_expr
  | Binop (op, left_expr, right_expr) ->
      let left_id, lhs_hint, left_stream =
        compile_rhs_expression c left_expr hint_for_function_name
      in
      let right_id, _, right_stream =
        compile_rhs_expression c right_expr
          (match op with "=" -> lhs_hint | _ -> hint_for_function_name)
      in
      let result_id, binop_stream =
        gen_id_and_stream (BinaryOp (left_id, String.trim op, right_id))
      in
      (result_id, None, left_stream >@ right_stream >@ binop_stream)
  | FunctionE fun_ast ->
      let name =
        match hint_for_function_name with
        | Some name -> Some name
        | None -> Some "anonymous"
      in
      no_hint @@ compile_closure c fun_ast name
  | Pexp inner_expr ->
      compile_rhs_expression c inner_expr hint_for_function_name
  | Clist [ callee_expr; Args (Elist arg_exprs) ] ->
      let callee_id, callee_hint, callee_code =
        compile_rhs_expression c callee_expr hint_for_function_name
      in
      let arg_ids, arg_codes =
        List.split
        @@ List.map
             (fun expr ->
               let id, _, stream =
                 compile_rhs_expression c expr hint_for_function_name
               in
               (id, stream))
             arg_exprs
      in
      let result_id = gen_local_id () in
      ( result_id,
        callee_hint,
        callee_code
        >@ List.concat @@ List.rev arg_codes
        >:: I (result_id, Ir.Call (callee_id, arg_ids)) )
  | _ ->
      let lhs_id, lhs_hint, lhs_stream = compile_lhs_expression c expr false in
      if lhs_id == -1 then unsupported_ast expr
      else
        let rhs_id = gen_local_id () in
        (rhs_id, lhs_hint, lhs_stream >:: I (rhs_id, Ir.Load lhs_id))

and compile_and_or (is_and : bool) (c : Ctxt.t) (left_expr : ast)
    (right_expr : ast) : Ir.local_id * stream =
  let left_id, _, left_stream = compile_rhs_expression c left_expr None in
  let right_id, _, right_stream = compile_rhs_expression c right_expr None in
  let result_id = gen_local_id () in
  let left_label = gen_label "and_or_left" in
  let right_label = gen_label "and_or_right" in
  let continue_label = gen_label "and_or_continue" in
  let join_label = gen_label "and_or_join" in
  let true_label, false_label =
    if is_and then (continue_label, join_label) else (join_label, continue_label)
  in
  ( result_id,
    left_stream
    >:: T (gen_local_id (), Ir.Br left_label)
    >:: L left_label
    >:: T (gen_local_id (), Ir.Cbr (left_id, true_label, false_label))
    >:: L continue_label >@ right_stream
    >:: T (gen_local_id (), Ir.Br right_label)
    >:: L right_label
    >:: T (gen_local_id (), Ir.Br join_label)
    >:: L join_label
    >:: I (result_id, Ir.Phi [ (left_label, left_id); (right_label, right_id) ])
  )

and compile_closure (c : Ctxt.t) (fun_ast : ast) (name_hint : string option) :
    Ir.local_id * stream =
  let fun_args, fun_statements =
    match fun_ast with
    | Fbody (Elist fun_args, Slist fun_statements) -> (fun_args, fun_statements)
    | _ -> failwith "unsupported ast for fun_ast"
  in
  let fun_args =
    List.map
      (function Ident name -> name | _ -> failwith "arguments must be Ident")
      fun_args
  in
  let _, fun_args_has_duplicates =
    List.fold_left
      (fun (seen, has_duplicates) s ->
        (s :: seen, has_duplicates || List.mem s seen))
      ([], false) fun_args
  in
  assert (not fun_args_has_duplicates);
  let inner_arg_val_ids = List.map (fun _ -> gen_local_id ()) fun_args in
  let inner_arg_var_ids = List.map (fun _ -> gen_local_id ()) fun_args in
  let arg_var_code =
    List.fold_left2
      (fun code val_id var_id ->
        code
        >:: I (var_id, Ir.Alloc)
        >:: I (gen_local_id (), Ir.Store (var_id, val_id)))
      [] inner_arg_val_ids inner_arg_var_ids
  in
  let c_no_duplicates =
    List.fold_left
      (fun c_no_duplicates (name, local_id) ->
        if Option.is_some (Ctxt.lookup_opt name c_no_duplicates) then
          c_no_duplicates
        else Ctxt.add c_no_duplicates name local_id)
      Ctxt.empty c
  in
  let inner_capture_ids = List.map (fun _ -> gen_local_id ()) c_no_duplicates in
  let inner_c =
    List.fold_left2
      (fun c (name, _) inner_id -> (name, inner_id) :: c)
      Ctxt.empty c_no_duplicates inner_capture_ids
  in
  let inner_c =
    List.fold_left2
      (fun c name inner_id -> (name, inner_id) :: c)
      inner_c fun_args inner_arg_var_ids
  in
  let inner_code =
    (snd @@ compile_statements inner_c None fun_statements) @ arg_var_code
  in
  let inner_code =
    match inner_code with
    | T (_, Ret _) :: _ -> inner_code
    | _ -> inner_code >:: T (gen_local_id (), Ret None)
  in
  let cfg, inner_fun_defs = cfg_of_stream inner_code in
  let fun_name = gen_global_id (Option.value name_hint ~default:"anonymous") in
  let fun_def : Ir.fun_def =
    {
      name = fun_name;
      capture_ids = inner_capture_ids;
      arg_ids = inner_arg_val_ids;
      cfg;
    }
  in
  let closure_id = gen_local_id () in
  ( closure_id,
    List.map (fun d -> F d) (fun_def :: inner_fun_defs)
    >:: I (closure_id, Ir.Alloc)
    >:: I
          ( gen_local_id (),
            Ir.StoreClosure (closure_id, fun_name, List.map snd c_no_duplicates)
          ) )

and compile_statement (c : Ctxt.t) (break_label : Ir.label option) (stmt : ast)
    : Ctxt.t * stream =
  match stmt with
  | Assign (Elist [ lhs_expr ], Elist [ rhs_expr ]) ->
      let lhs_id, lhs_hint, lhs_code = compile_lhs_expression c lhs_expr true in
      let rhs_id, _, rhs_code = compile_rhs_expression c rhs_expr lhs_hint in
      ( c,
        lhs_code >@ rhs_code >:: I (gen_local_id (), Ir.Store (lhs_id, rhs_id))
      )
  | Lnames (Elist [ Ident lhs_name ]) ->
      let var_id = gen_local_id () in
      (Ctxt.add c lhs_name var_id, [ I (var_id, Ir.Alloc) ])
  | Lassign (Elist [ Ident lhs_name ], Elist [ rhs_expr ]) ->
      compile_statements c break_label
        [
          Lnames (Elist [ Ident lhs_name ]);
          Assign (Elist [ Ident lhs_name ], Elist [ rhs_expr ]);
        ]
  | Clist _ ->
      let _, _, stream = compile_rhs_expression c stmt None in
      (c, stream)
  | If1 (cond, body) ->
      compile_statement c break_label (If3 (cond, body, Slist []))
  | If2 (cond, then_body, else_body) ->
      compile_statement c break_label
        (If3 (cond, then_body, Slist [ Elseif (Bool "true", else_body) ]))
  | If3 (first_cond, first_body, Slist elseifs) ->
      let branches =
        List.map
          (function
            | Elseif (cond, Slist body) -> (cond, body)
            | _ -> failwith "expected Elseif")
          (Elseif (first_cond, first_body) :: elseifs)
      in
      let condition_labels =
        List.map (fun _b -> gen_label "if_condition") branches
      in
      let body_labels = List.map (fun _b -> gen_label "if_body") branches in
      let join_label = gen_label "if_join" in
      let branch_codes =
        List.combine condition_labels (List.tl condition_labels @ [ join_label ])
        |> List.combine body_labels |> List.combine branches
        |> List.rev_map
             (fun
               ( (condition, body),
                 (body_label, (condition_label, next_condition_label)) )
             ->
               let condition_id, _, condition_code =
                 compile_rhs_expression c condition None
               in
               let body_stream =
                 compile_statements c break_label body
                 |> snd
                 |> add_terminator_if_needed (Ir.Br join_label)
               in
               [] >:: L condition_label >@ condition_code
               >:: T
                     ( gen_local_id (),
                       Ir.Cbr (condition_id, body_label, next_condition_label)
                     )
               >:: L body_label >@ body_stream)
        |> List.flatten
      in
      ( c,
        [ T (gen_local_id (), Ir.Br (List.hd condition_labels)) ]
        >@ branch_codes >:: L join_label )
  | If4 (first_cond, first_body, Slist elseifs, else_body) ->
      compile_statement c break_label
        (If3
           ( first_cond,
             first_body,
             Slist (elseifs @ [ Elseif (Bool "true", else_body) ]) ))
  | Return (Elist []) -> (c, [ T (gen_local_id (), Ir.Ret None) ])
  | Return (Elist [ expr ]) ->
      let expr_id, _, expr_code = compile_rhs_expression c expr None in
      (c, expr_code >:: T (gen_local_id (), Ir.Ret (Some expr_id)))
  | Function (FNlist [ Ident name ], fun_body) ->
      let closure_id, closure_code = compile_closure c fun_body (Some name) in
      let name_id, _, name_code = compile_lhs_expression c (Ident name) true in
      ( c,
        closure_code >@ name_code
        >:: I (gen_local_id (), Ir.Store (name_id, closure_id)) )
  | For1 (Ident var_name, start_expr, end_expr, Slist statements) ->
      let start_id, _, start_code = compile_rhs_expression c start_expr None in
      let end_id, _, end_code = compile_rhs_expression c end_expr None in
      let val_id = gen_local_id () in
      let var_id = gen_local_id () in
      let step_id = gen_local_id () in
      let next_val_id = gen_local_id () in
      let continue_id = gen_local_id () in
      let init_end_label = gen_label "for_init_end" in
      let head_label = gen_label "for_head" in
      let body_start_label = gen_label "for_body_start" in
      let body_end_label = gen_label "for_body_end" in
      let join_label = gen_label "for_join" in
      ( c,
        start_code >@ end_code
        >:: I (step_id, Ir.NumberConstant (Pico_number.of_int 1))
        >:: T (gen_local_id (), Ir.Br init_end_label)
        >:: L init_end_label
        >:: T (gen_local_id (), Ir.Br head_label)
        >:: L head_label
        >:: I
              ( val_id,
                Ir.Phi
                  [ (init_end_label, start_id); (body_end_label, next_val_id) ]
              )
        >:: I (continue_id, Ir.BinaryOp (val_id, "<=", end_id))
        >:: T
              ( gen_local_id (),
                Ir.Cbr (continue_id, body_start_label, join_label) )
        >:: L body_start_label
        >:: I (var_id, Ir.Alloc)
        >:: I (gen_local_id (), Ir.Store (var_id, val_id))
        >@ snd
           @@ compile_statements
                (Ctxt.add c var_name var_id)
                (Some join_label) statements
        >:: I (next_val_id, Ir.BinaryOp (val_id, "+", step_id))
        >:: T (gen_local_id (), Ir.Br body_end_label)
        >:: L body_end_label
        >:: T (gen_local_id (), Ir.Br head_label)
        >:: L join_label )
  | Break -> (c, [ T (gen_local_id (), Ir.Br (Option.get break_label)) ])
  | _ -> unsupported_ast stmt

and compile_statements (c : Ctxt.t) (break_label : Ir.label option)
    (statements : ast list) : Ctxt.t * stream =
  List.fold_left
    (fun (old_c, old_code) stmt ->
      let new_c, extra_code = compile_statement old_c break_label stmt in
      (new_c, extra_code @ old_code))
    (c, []) statements

let compile_top_level_ast (ast : ast) : stream =
  let statements =
    match ast with
    | Lua_parser.Ast.Slist l -> l
    | _ -> failwith "AST should have an Slist at the top level"
  in
  let _, stream = compile_statements Ctxt.empty None statements in
  stream >@ [ T (gen_local_id (), Ir.Ret None) ]

let load_program filename =
  BatFile.with_file_in filename (fun f ->
      f |> Batteries.IO.to_input_channel |> Lua_parser.Parse.parse_from_chan)