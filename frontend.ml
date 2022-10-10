open Lua_parser.Ast

type stream_el =
  | L of Ir.label
  | I of Ir.local_id * Ir.instruction
  | T of Ir.terminator
[@@deriving show]

type stream = stream_el list [@@deriving show]

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

let gen_label : string -> string =
  let i = ref 0 in
  fun s ->
    incr i;
    Printf.sprintf "%s_%d" s !i

let gen_id_and_stream (insn : Ir.instruction) : Ir.local_id * stream =
  let id = gen_local_id () in
  (id, [ I (id, insn) ])

let compile_lhs_expression (c : Ctxt.t) (expr : ast) : Ir.local_id * stream =
  match expr with
  | Ident name -> (
      match Ctxt.lookup_opt name c with
      | Some id -> (id, [])
      | None -> gen_id_and_stream (Ir.GetOrCreateGlobal name))
  | _ -> (-1, [])

let compile_rhs_expression (c : Ctxt.t) (expr : ast) : Ir.local_id * stream =
  match expr with
  | Table (Elist []) ->
      let id = gen_local_id () in
      (id, [ I (gen_local_id (), Ir.StoreEmptyTable id); I (id, Ir.Alloc) ])
  | Number s -> gen_id_and_stream (Ir.NumberConstant (Pico_number.of_string s))
  | Bool "true" -> gen_id_and_stream (Ir.BoolConstant true)
  | Bool "false" -> gen_id_and_stream (Ir.BoolConstant false)
  | Ident _ ->
      let var_id, var_stream = compile_lhs_expression c expr in
      let val_id = gen_local_id () in
      (val_id, I (val_id, Ir.Load var_id) :: var_stream)
  | _ -> (-1, [])

let rec compile_statement (c : Ctxt.t) (stmt : ast) : Ctxt.t * stream =
  match stmt with
  | Assign (Elist [ lhs_expr ], Elist [ rhs_expr ]) ->
      let lhs_id, lhs_code = compile_lhs_expression c lhs_expr in
      let rhs_id, rhs_code = compile_rhs_expression c rhs_expr in
      ( c,
        (I (gen_local_id (), Ir.Store (lhs_id, rhs_id)) :: rhs_code) @ lhs_code
      )
  | Clist [ callee_expr; Args (Elist arg_exprs) ] ->
      let callee_id, callee_code = compile_rhs_expression c callee_expr in
      let arg_ids, arg_codes =
        List.split @@ List.map (compile_rhs_expression c) arg_exprs
      in
      ( c,
        I (gen_local_id (), Ir.Call (callee_id, arg_ids))
        :: (List.concat @@ List.rev arg_codes)
        @ callee_code )
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
               List.flatten
                 [
                   [ T (Ir.Br join_label) ];
                   snd (compile_statements c body);
                   [
                     L body_label;
                     T (Ir.Cbr (condition_id, body_label, next_condition_label));
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
            [ T (Ir.Br (List.hd condition_labels)) ];
          ] )
  | If4 (first_cond, first_body, Slist elseifs, else_body) ->
      compile_statement c
        (If3
           ( first_cond,
             first_body,
             Slist (elseifs @ [ Elseif (Bool "true", else_body) ]) ))
  | _ -> (c, [])

and compile_statements (c : Ctxt.t) (statements : ast list) : Ctxt.t * stream =
  List.fold_left
    (fun (old_c, old_code) stmt ->
      let new_c, extra_code = compile_statement c stmt in
      (new_c, extra_code @ old_code))
    (c, []) statements

let load_program filename =
  BatFile.with_file_in filename (fun f ->
      f |> Batteries.IO.to_input_channel |> Lua_parser.Parse.parse_from_chan)

let () =
  let example_program = load_program "celeste-minimal.lua" in
  let statements =
    match example_program with
    | Slist statements -> statements
    | _ -> failwith "expected SList"
  in
  let statements =
    statements
    |> List.find_map (function
         | Function
             (FNlist (Ident "next_room" :: _), Fbody (_, Slist statements)) ->
             Some statements
         | _ -> None)
  in
  let statements =
    match statements with
    | Some statements -> statements
    | None -> failwith "target function not found"
  in
  let result =
    snd
    @@ List.fold_left
         (fun (c, code) s ->
           let c, stmt_code = compile_statement c s in
           (c, stmt_code @ code))
         (Ctxt.empty, []) statements
  in
  Lua_parser.Pp_ast.pp_ast_show (Slist statements);
  Printf.printf "%s\n" @@ show_stream result
