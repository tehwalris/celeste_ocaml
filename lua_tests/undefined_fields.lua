x = {}
__print(x.y)

x = {}
add(x, 1)
__print(x[1])
__print(x[2])
__print(x[3])
__print(x[2] == nil)

x = {y='a', z=nil}
__print(x.y)
__print(x.z)
__print(x.a)
__print(x.a == nil)
__print(x.z == nil)
__print(x.a == x.z)

__print(global_which_does_not_exist)
__print(x.a == global_which_does_not_exist)