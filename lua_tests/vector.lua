x_values = {}
add(x_values, 6)
add(x_values, 3)
add(x_values, 8)
x = __new_vector(x_values)

y_values = {}
add(y_values, 2)
add(y_values, 4)
add(y_values, 6)
y = __new_vector(y_values)

function f(v)
  return v + 1
end

__print(x)
__print(x + y)
__print(x + 3)
__print(3 + x)
__print((x + 0.5) + (y + 0.5))
x = y
__print(x + y)
__print(f(x))

-- Expected output option:
-- V[6, 3, 8]
-- V[8, 7, 14]
-- V[9, 6, 11]
-- V[9, 6, 11]
-- V[9, 8, 15]
-- V[4, 8, 12]
-- V[3, 5, 7]