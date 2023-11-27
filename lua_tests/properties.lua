a = { b = { c = 0, d = nil } }
print(a.b.c)
a.b.c = 123
print(a.b.c)
print(a.b.d)
a.b.d = 456
print(a.b.d)