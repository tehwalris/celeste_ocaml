let man = Oct.manager_alloc () in

let var_x = Apron.Var.of_string "x" in
let var_y = Apron.Var.of_string "y" in

let env = Apron.Environment.make [| var_x; var_y |] [||] in
Format.printf "env = %a@." (fun x -> Apron.Environment.print x) env;

let tab = Apron.Parser.lincons1_of_lstring env [ "x>=5"; "x<10" ] in
Format.printf "tab = %a@." (fun x -> Apron.Lincons1.array_print x) tab;
let abs = Apron.Abstract1.of_lincons_array man env tab in
Format.printf "abs = %a@." Apron.Abstract1.print abs;

let tab = Apron.Parser.lincons1_of_lstring env [ "x>=5"; "x<10"; "y < x" ] in
Format.printf "tab = %a@\n" (fun x -> Apron.Lincons1.array_print x) tab;
let abs = Apron.Abstract1.of_lincons_array man env tab in
Format.printf "abs = %a@." Apron.Abstract1.print abs;

let x = Apron.Abstract1.bound_variable man abs var_x in
let y = Apron.Abstract1.bound_variable man abs var_y in
Format.printf "x = %a@.y = %a@." Apron.Interval.print x Apron.Interval.print y