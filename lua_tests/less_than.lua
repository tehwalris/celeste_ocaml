low_values = {}
add(low_values, 0)
add(low_values, 1)
low = __new_vector(low_values)

high_values = {}
add(high_values, 1.5)
add(high_values, 1.8)
high = __new_vector(high_values)

__print(0 < 1.5)
__print(1 < 1.8)
__print(low < 1.5)
__print(1 < high)
__print(low < high)
__print((0 + 1) < 1.5)
__print((1 + 1) < 1.8)
__print((low + 1) < high)
__print(low)
__print(flr(high))

-- Expected output option:
-- true
-- true
-- true
-- true
-- true
-- true
-- false
-- V[true, false]
-- V[0, 1]
-- 1