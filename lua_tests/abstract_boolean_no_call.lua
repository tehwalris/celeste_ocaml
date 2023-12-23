v = __new_unknown_boolean()
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