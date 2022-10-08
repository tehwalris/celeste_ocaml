open Lua_parser.Ast

type stream_el = L of Ir.label | I of Ir.instruction [@@deriving show]
type stream = stream_el list [@@deriving show]

module Ctxt = struct
  type t = (string * Ir.local_id) list

  let empty = []
  let add (c : t) (name : string) (value : Ir.local_id) : t = (name, value) :: c
  let lookup : string -> t -> Ir.local_id = List.assoc
end

let compile_statement (c : Ctxt.t) (s : ast) : Ctxt.t * stream = (c, [])

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
