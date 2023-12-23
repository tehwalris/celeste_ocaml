x = {}
x.a = 1
x.b = 7
x.c = 'hello'
print(x.a)
print(x.b)
print(x.c)
x.c = 'world'
print(x.c)

y = {a = 1, b = 7, c = 'hello'}
print(y.a)
print(y.b)
print(y.c)
y.c = 'world'
print(y.c)

z = {}
add(z, 1)
add(z, 7)
add(z, 'hello')
print(z[1])
print(z[2])
print(z[3])
z[3] = 'world'
print(z[3])