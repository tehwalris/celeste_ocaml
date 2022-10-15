open Lua_parser.Ast

type stream_el =
  | L of Ir.label
  | I of Ir.local_id * Ir.instruction
  | T of Ir.local_id * Ir.terminator
  | F of Ir.fun_def
[@@deriving show]

type stream = stream_el list [@@deriving show]

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
  ( {
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

let gen_local_id : unit -> int =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

let gen_global_id : string -> string =
  let i = ref 0 in
  fun s ->
    incr i;
    Printf.sprintf "%s_%d" s !i

let gen_label : string -> string =
  let i = ref 0 in
  fun s ->
    incr i;
    Printf.sprintf "%s_%d" s !i

let gen_id_and_stream (insn : Ir.instruction) : Ir.local_id * stream =
  let id = gen_local_id () in
  (id, [ I (id, insn) ])

let rec compile_lhs_expression (c : Ctxt.t) (expr : ast)
    (create_if_missing : bool) : Ir.local_id * stream =
  match expr with
  | Ident name -> (
      match Ctxt.lookup_opt name c with
      | Some id -> (id, [])
      | None -> gen_id_and_stream (Ir.GetGlobal (name, create_if_missing)))
  | Clist [ lhs_expr; Key1 rhs_expr ] ->
      let lhs_id, lhs_stream = compile_rhs_expression c lhs_expr in
      let rhs_id, rhs_stream = compile_rhs_expression c rhs_expr in
      let result_id = gen_local_id () in
      ( result_id,
        I (result_id, Ir.GetIndex (lhs_id, rhs_id, create_if_missing))
        :: rhs_stream
        @ lhs_stream )
  | Clist [ lhs_expr; Key2 (Ident field_name) ] ->
      let lhs_id, lhs_stream = compile_rhs_expression c lhs_expr in
      let result_id = gen_local_id () in
      ( result_id,
        I (result_id, Ir.GetField (lhs_id, field_name, create_if_missing))
        :: lhs_stream )
  | _ ->
      Lua_parser.Pp_lua.pp_lua expr;
      Printf.printf "\n";
      Lua_parser.Pp_ast.pp_ast_show expr;
      (-1, [])

and compile_rhs_expression (c : Ctxt.t) (expr : ast) : Ir.local_id * stream =
  match expr with
  | Table (Elist assignments) ->
      let table_id = gen_local_id () in
      ( table_id,
        List.concat_map
          (function
            | Assign (Ident field_name, value_expr) ->
                let field_id = gen_local_id () in
                let value_id, value_code =
                  compile_rhs_expression c value_expr
                in
                [
                  I (gen_local_id (), Ir.Store (field_id, value_id));
                  I (field_id, Ir.GetField (table_id, field_name, true));
                ]
                @ value_code
            | _ -> failwith "only Assign is allowed in Table expression")
          (List.rev assignments)
        @ [
            I (gen_local_id (), Ir.StoreEmptyTable table_id);
            I (table_id, Ir.Alloc);
          ] )
  | Number s -> gen_id_and_stream (Ir.NumberConstant (Pico_number.of_string s))
  | Bool "true" -> gen_id_and_stream (Ir.BoolConstant true)
  | Bool "false" -> gen_id_and_stream (Ir.BoolConstant false)
  | Bool "nil" -> gen_id_and_stream Ir.NilConstant
  | String s -> gen_id_and_stream (Ir.StringConstant s)
  | Unop (op, inner_expr) ->
      let inner_id, inner_stream = compile_rhs_expression c inner_expr in
      let result_id, result_stream =
        gen_id_and_stream (UnaryOp (String.trim op, inner_id))
      in
      (inner_id, result_stream @ inner_stream)
  | Binop (op, left_expr, right_expr) ->
      let left_id, left_stream = compile_rhs_expression c left_expr in
      let right_id, right_stream = compile_rhs_expression c right_expr in
      let result_id, binop_stream =
        gen_id_and_stream (BinaryOp (left_id, String.trim op, right_id))
      in
      (result_id, binop_stream @ right_stream @ left_stream)
  | FunctionE fun_ast -> compile_closure c fun_ast (Some "anonymous")
  | Pexp inner_expr -> compile_rhs_expression c inner_expr
  | Clist [ callee_expr; Args (Elist arg_exprs) ] ->
      let callee_id, callee_code = compile_rhs_expression c callee_expr in
      let arg_ids, arg_codes =
        List.split @@ List.map (compile_rhs_expression c) arg_exprs
      in
      let result_id = gen_local_id () in
      ( result_id,
        I (result_id, Ir.Call (callee_id, arg_ids))
        :: (List.concat @@ List.rev arg_codes)
        @ callee_code )
  | _ ->
      let lhs_id, lhs_stream = compile_lhs_expression c expr false in
      if lhs_id == -1 then (
        Lua_parser.Pp_lua.pp_lua expr;
        Printf.printf "\n";
        Lua_parser.Pp_ast.pp_ast_show expr;
        (-1, []))
      else
        let rhs_id = gen_local_id () in
        (rhs_id, I (rhs_id, Ir.Load lhs_id) :: lhs_stream)

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
        [ I (gen_local_id (), Ir.Store (var_id, val_id)); I (var_id, Ir.Alloc) ]
        @ code)
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
    (snd @@ compile_statements inner_c fun_statements) @ arg_var_code
  in
  let inner_code =
    match inner_code with
    | T (_, Ret _) :: _ -> inner_code
    | _ -> T (gen_local_id (), Ret None) :: inner_code
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
    List.flatten
      [
        [
          I
            ( gen_local_id (),
              Ir.StoreClosure
                (closure_id, fun_name, List.map snd c_no_duplicates) );
          I (closure_id, Ir.Alloc);
        ];
        List.map (fun d -> F d) (fun_def :: inner_fun_defs);
      ] )

and compile_statement (c : Ctxt.t) (stmt : ast) : Ctxt.t * stream =
  match stmt with
  | Assign (Elist [ lhs_expr ], Elist [ rhs_expr ]) ->
      let lhs_id, lhs_code = compile_lhs_expression c lhs_expr true in
      let rhs_id, rhs_code = compile_rhs_expression c rhs_expr in
      ( c,
        (I (gen_local_id (), Ir.Store (lhs_id, rhs_id)) :: rhs_code) @ lhs_code
      )
  | Lnames (Elist [ Ident lhs_name ]) ->
      let var_id = gen_local_id () in
      (Ctxt.add c lhs_name var_id, [ I (var_id, Ir.Alloc) ])
  | Lassign (Elist [ Ident lhs_name ], Elist [ rhs_expr ]) ->
      compile_statements c
        [
          Lnames (Elist [ Ident lhs_name ]);
          Assign (Elist [ Ident lhs_name ], Elist [ rhs_expr ]);
        ]
  | Clist _ -> (c, snd @@ compile_rhs_expression c stmt)
  | If1 (cond, body) -> compile_statement c (If3 (cond, body, Slist []))
  | If2 (cond, then_body, else_body) ->
      compile_statement c
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
               let condition_id, condition_code =
                 compile_rhs_expression c condition
               in
               let body_stream = snd (compile_statements c body) in
               let extra_terminator =
                 match body_stream with
                 | T (_, Ret _) :: _ -> []
                 | _ -> [ T (gen_local_id (), Ir.Br join_label) ]
               in
               List.flatten
                 [
                   extra_terminator;
                   body_stream;
                   [
                     L body_label;
                     T
                       ( gen_local_id (),
                         Ir.Cbr (condition_id, body_label, next_condition_label)
                       );
                   ];
                   condition_code;
                   [ L condition_label ];
                 ])
        |> List.flatten
      in
      ( c,
        List.flatten
          [
            [ L join_label ];
            branch_codes;
            [ T (gen_local_id (), Ir.Br (List.hd condition_labels)) ];
          ] )
  | If4 (first_cond, first_body, Slist elseifs, else_body) ->
      compile_statement c
        (If3
           ( first_cond,
             first_body,
             Slist (elseifs @ [ Elseif (Bool "true", else_body) ]) ))
  | Return (Elist []) -> (c, [ T (gen_local_id (), Ir.Ret None) ])
  | Return (Elist [ expr ]) ->
      let expr_id, expr_code = compile_rhs_expression c expr in
      (c, T (gen_local_id (), Ir.Ret (Some expr_id)) :: expr_code)
  | Function (FNlist [ Ident name ], fun_body) ->
      let closure_id, closure_code = compile_closure c fun_body (Some name) in
      let name_id, name_code = compile_lhs_expression c (Ident name) true in
      ( c,
        I (gen_local_id (), Ir.Store (name_id, closure_id))
        :: (name_code @ closure_code) )
  | _ ->
      Lua_parser.Pp_lua.pp_lua stmt;
      Printf.printf "\n";
      Lua_parser.Pp_ast.pp_ast_show stmt;
      (c, [])

and compile_statements (c : Ctxt.t) (statements : ast list) : Ctxt.t * stream =
  List.fold_left
    (fun (old_c, old_code) stmt ->
      let new_c, extra_code = compile_statement old_c stmt in
      (new_c, extra_code @ old_code))
    (c, []) statements

let load_program filename =
  BatFile.with_file_in filename (fun f ->
      f |> Batteries.IO.to_input_channel |> Lua_parser.Parse.parse_from_chan)

let () =
  let example_program = load_program "celeste-standard-syntax.lua" in
  let statements =
    match example_program with
    | Slist statements -> statements
    | _ -> failwith "expected SList"
  in
  let _, result = compile_statements Ctxt.empty statements in
  (* Lua_parser.Pp_ast.pp_ast_show (Slist statements); *)
  (* Printf.printf "%s\n" @@ show_stream result *)
  ()