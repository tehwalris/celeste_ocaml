let man = Oct.manager_alloc () in

let var_y = Apron.Var.of_string "y" in
let var_target = Apron.Var.of_string "target" in

let env = Apron.Environment.make [| var_y; var_target |] [||] in
Format.printf "env = %a@." (fun x -> Apron.Environment.print x) env;

let tab =
  Apron.Parser.lincons1_of_lstring env
    [ "target>=0"; "target<=32"; "y>=40"; "y<=128"; "y<target+16" ]
in
Format.printf "tab = %a@." (fun x -> Apron.Lincons1.array_print x) tab;
let abs = Apron.Abstract1.of_lincons_array man env tab in
Format.printf "abs = %a@." Apron.Abstract1.print abs;

let range_y = Apron.Abstract1.bound_variable man abs var_y in
let range_target = Apron.Abstract1.bound_variable man abs var_target in
Format.printf "var_y = %a@.var_target = %a@." Apron.Interval.print range_y
  Apron.Interval.print range_target
