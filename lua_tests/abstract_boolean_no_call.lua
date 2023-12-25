v = __new_unknown_boolean()
if v then
  __print("a")
else
  __print("b")
end
if v then
  __print("c")
else
  __print("d")
end

-- Expected output option:
-- a
-- c

-- Expected output option:
-- a
-- d

-- Expected output option:
-- b
-- c

-- Expected output option:
-- b
-- d