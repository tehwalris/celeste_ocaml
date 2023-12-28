objects = {}

function init_object(type, x, y)
    local obj = {}
    obj.type = type
    obj.collideable = true

    obj.x = x
    obj.y = y
    obj.hitbox = {x = 0, y = 0, w = 8, h = 8}

    obj.collide = function(type, ox, oy)
        local other
        for i = 1, count(objects) do
            other = objects[i]
            if
                other ~= nil and other.type == type and other ~= obj and other.collideable and
                    other.x + other.hitbox.x + other.hitbox.w > obj.x + obj.hitbox.x + ox and
                    other.y + other.hitbox.y + other.hitbox.h > obj.y + obj.hitbox.y + oy and
                    other.x + other.hitbox.x < obj.x + obj.hitbox.x + obj.hitbox.w + ox and
                    other.y + other.hitbox.y < obj.y + obj.hitbox.y + obj.hitbox.h + oy
              then
                return other
            end
        end
        return nil
    end

    add(objects, obj)
    return obj
end

player = {}
other_thing = {}

init_object(player, 0, 0)
init_object(other_thing, 10, 10)

for i=1,256 do
    if i % 10 == 0 then
        __debug("i", i)
    end
    foreach(objects, function(obj)
        obj.collide(other_thing, 0, 0)
    end)
    __hint_normalize()
end