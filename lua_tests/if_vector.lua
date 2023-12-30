low_values = {}
add(low_values, 0)
add(low_values, 1)
low = __new_vector(low_values)

high_values = {}
add(high_values, 1.5)
add(high_values, 1.8)
high = __new_vector(high_values)

__print(low)
if (low + 1) < high then
  __print("below")
  __print(low)
end

-- Expected output option:
-- V[0, 1]
-- below
-- 0

-- Expected output option:
-- V[0, 1]