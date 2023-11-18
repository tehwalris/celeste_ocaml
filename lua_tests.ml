open Lua_api

let check_lua_status state status =
  match status with
  | Lua.LUA_OK -> ()
  | _ ->
      let err_msg =
        match Lua.tostring state (-1) with
        | Some msg ->
            Lua.pop state 1;
            msg
        | None -> "Unknown error"
      in
      failwith err_msg

let handle_print_from_lua state =
  if Lua.gettop state <> 1 then failwith "Wrong number of arguments";
  let s = Option.get @@ Lua.tostring state 1 in
  Printf.printf "Lua wants to print: %s\n" s;
  0

let%test_unit _ =
  let state = LuaL.newstate () in
  LuaL.openlibs state;
  Lua.pushcfunction state handle_print_from_lua;
  Lua.setglobal state "print";
  check_lua_status state @@ LuaL.loadfile state "hello_world.lua";
  check_lua_status state @@ Lua.pcall state 0 0 0
