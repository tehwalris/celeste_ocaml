open Lua_parser.Ast

type stream_el = L of Ir.label | I of Ir.local_id * Ir.instruction
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

let compile_statement (c : Ctxt.t) (stmt : ast) : Ctxt.t * stream =
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
  | _ -> (c, [])

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
             (FNlist (Ident "title_screen" :: _), Fbody (_, Slist statements))
           ->
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
