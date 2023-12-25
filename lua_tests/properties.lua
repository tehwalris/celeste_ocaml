a = { b = { c = 0, d = nil } }
__print(a.b.c)
a.b.c = 123
__print(a.b.c)
__print(a.b.d)
a.b.d = 456
__print(a.b.d)