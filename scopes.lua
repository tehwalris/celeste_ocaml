x = 0
y = 7

function g()
  y = 6

  local y = 4

  local f = function(x)
    print(x)
    x = 2
    print(x)
    print(y)
    y = 3
  end

  local h = function()
    local x = 5
    f(x)
    print(y)
    print(x)
  end

  y = 5

  return h
end

print(y)
f = g()
f()
print(y)

-- prints:
-- 7
-- 5
-- 2
-- 5
-- 3
-- 5
-- 6
