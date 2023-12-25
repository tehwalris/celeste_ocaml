x = {}
x.a = 1
x.b = 7
x.c = 'hello'
__print(x.a)
__print(x.b)
__print(x.c)
x.c = 'world'
__print(x.c)

y = {a = 1, b = 7, c = 'hello'}
__print(y.a)
__print(y.b)
__print(y.c)
y.c = 'world'
__print(y.c)

z = {}
add(z, 1)
add(z, 7)
add(z, 'hello')
__print(z[1])
__print(z[2])
__print(z[3])
z[3] = 'world'
__print(z[3])