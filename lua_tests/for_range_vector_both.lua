low_values = {}
add(low_values, 0)
add(low_values, 1)
low = __new_vector(low_values)

high_values = {}
add(high_values, 1.5)
add(high_values, 1.8)
high = __new_vector(high_values)

for i=low,high do
  __print("low")
  __print(low)
  __print("i")
  __print(i)
end
__print("low after loop")
__print(low)

-- Expected output option:
-- low
-- V[0, 1]
-- i
-- V[0, 1]
-- low after loop
-- 1

-- Expected output option:
-- low
-- V[0, 1]
-- i
-- V[0, 1]
-- low
-- 0
-- i
-- 1
-- low after loop
-- 0