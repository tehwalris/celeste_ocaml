function get_name_of_type(type)
  if type == nil then
    return "nil"
  elseif type == player then
    return "player"
  elseif type == player_spawn then
    return "player_spawn"
  elseif type == spring then
    return "spring"
  elseif type == balloon then
    return "baloon"
  elseif type == fall_floor then
    return "fall_floor"
  elseif type == smoke then
    return "smoke"
  elseif type == fruit then
    return "fruit"
  elseif type == fly_fruit then
    return "fly_fruit"
  elseif type == lifeup then
    return "lifeup"
  elseif type == fake_wall then
    return "fake_wall"
  elseif type == key then
    return "key"
  elseif type == chest then
    return "chest"
  elseif type == platform then
    return "platform"
  elseif type == message then
    return "message"
  elseif type == big_chest then
    return "big_chest"
  elseif type == orb then
    return "orb"
  elseif type == flag then
    return "flag"
  elseif type == room_title then
    return "room_title"
  else
    error("unknown type", type)
  end
end

function find_object(cb)
  if count(objects) == 0 then
    return nil
  end
  for i=1,count(objects) do
    local o = objects[i]
    if cb(o) then
      return o
    end
  end
  return nil
end

function find_player_spawn()
  return find_object(function(o) return o.type == player_spawn end)
end

function inspect_celeste()
  always_print("count(objects)", count(objects))
end