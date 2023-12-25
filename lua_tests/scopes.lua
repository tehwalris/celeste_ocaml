x = 0
y = 7

function g()
  y = 6

  local y = 4

  local f = function(x)
    __print(x)
    x = 2
    __print(x)
    __print(y)
    y = 3
  end

  local h = function()
    local x = 5
    f(x)
    __print(y)
    __print(x)
  end

  y = 5

  return h
end

__print(y)
f = g()
f()
__print(y)