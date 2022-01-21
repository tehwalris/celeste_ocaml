function f(x)
  if x then
    local y = 5
  else
    y = 7
  end
end

f(true)
print(y)
f(false)
print(y)

-- prints:
-- nil
-- 7
