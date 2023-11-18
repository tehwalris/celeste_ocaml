open Compiler_lib
open Compiler_lib.Frontend
open Lua_parser.Ast

let () =
  let example_program = load_program "celeste-standard-syntax.lua" in
  let statements =
    match example_program with
    | Slist statements -> statements
    | _ -> failwith "expected SList"
  in
  let target_fun_name = "set_hair_color" in
  let target_fun_body =
    Option.get
    @@ List.find_map
         (function
           | Function (FNlist [ Ident name ], fun_body)
             when name = target_fun_name ->
               Some fun_body
           | Assign (Elist [ Ident name ], Elist [ FunctionE fun_body ])
             when name = target_fun_name ->
               Some fun_body
           | _ -> None)
         statements
  in
  Lua_parser.Pp_ast.pp_ast_show target_fun_body;
  let _, stream =
    compile_closure Ctxt.empty target_fun_body (Some target_fun_name)
  in
  let compiled_fun_name =
    stream
    |> List.find_map (function
         | I (_, Ir.StoreClosure (_, s, _)) -> Some s
         | _ -> None)
    |> Option.get
  in
  Printf.printf "%s\n" compiled_fun_name;
  let _, fun_defs =
    stream |> add_terminator_if_needed (Ir.Ret None) |> cfg_of_stream
  in
  let target_fun_def =
    List.find (fun (d : Ir.fun_def) -> d.name == compiled_fun_name) fun_defs
  in
  Printf.printf "%s\n" @@ Ir.show_fun_def target_fun_def;
  Flow.FlowGraphDot.output_graph (open_out "flow_graph.dot")
  @@ Flow.flow_graph_of_cfg target_fun_def.cfg;
  ()