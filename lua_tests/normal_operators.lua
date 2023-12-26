__print(1 + 1)
__print(5 - 7)
__print(7 - 5)
__print(3 * 4)
__print(12 / 3)
__print(2 - 2 / 4 + 0.5)
__print(12 % 5)
__print(12.5 % 5 + 0.5)
__print(-7)
__print(-(-7))
__print(1 < 2)
__print(1 <= 2)
__print(1 > 2)
__print(1 >= 2)
__print(1 == 2)
__print(1 ~= 2)
__print(not (1 == 2))
__print(not true)
__print("wal".."rus")
__print("12"..3)
__print((1).."23")
__print(((1).."23") == "123")

empty_table_a = {} 
empty_table_b = {}
object_table_a = {x=1}
object_table_b = {x=1}
array_table_a = {}
array_table_a[1] = 'a'
array_table_b = {}
array_table_b[1] = 'b'
function_a = function () end 
function_b = function () end
__print(empty_table_a == empty_table_a)
__print(empty_table_a ~= empty_table_a)
__print(empty_table_a == empty_table_b)
__print(empty_table_a ~= empty_table_b)
__print(object_table_a == object_table_a)
__print(object_table_a ~= object_table_a)
__print(object_table_a == object_table_b)
__print(object_table_a ~= object_table_b)
__print(array_table_a == array_table_a)
__print(array_table_a ~= array_table_a)
__print(array_table_a == array_table_b)
__print(array_table_a ~= array_table_b)
__print(empty_table_a == object_table_a)
__print(empty_table_a == array_table_a)
__print(object_table_a == array_table_a)
__print(function_a == function_a)
__print(function_a ~= function_a)
__print(function_a == function_b)
__print(function_a ~= function_b)