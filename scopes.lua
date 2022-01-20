x = 0
y = 7

function g()
  y = 6

  local y = 4

  local f = function()
    print(y)
    y = 3
  end

  local h = function()
    f()
    print(y)
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
-- 3
-- 6