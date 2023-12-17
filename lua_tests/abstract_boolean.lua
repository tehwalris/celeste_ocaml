function f(v)
  if v then
    print("a")
  else
    print("b")
  end
  if v then
    print("c")
  else
    print("d")
  end
end

f(__new_unknown_boolean())

-- Expected output option:
-- a
-- c

-- TODO Expected output option:
-- a
-- d

-- TODO Expected output option:
-- b
-- c

-- TODO Expected output option:
-- b
-- d