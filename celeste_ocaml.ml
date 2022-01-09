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

(*
rem0 = ...
arg0 = rem0 + 0.5
amount0 = flr(arg0)
rem1 = rem0 - amount0

flr([12.2 15.6]) = [12 15]
flr([12.2 13[) = [12 12]
flr([13 14[) = [13 13]
flr([14 15[) = [14 14]
flr([15 15.6[) = [15 15]

a - flr(a) = b
flr(a) = a - b

flr(a) <= a
b >= 0 && b < 1

a - 1 < flr(a) <= a
*)

(*

player
player_spawn
spring
balloon
fall_floor
smoke - dead
fruit
fly_fruit
lifeup - dead
fake_wall
key
chest
platform
message - dead
big_chest
orb
flag - dead
room_title - dead

*)