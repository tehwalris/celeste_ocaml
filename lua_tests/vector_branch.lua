x_values = {}
add(x_values, 6)
add(x_values, 3)
add(x_values, 8)
x = __new_vector(x_values)

__print(x)
if x < 5 then
  __print(x)
else
  __print(x)
end

-- Expected output option:
-- V[6, 3, 8]
-- 3

-- Expected output option:
-- V[6, 3, 8]
-- V[6, 8]