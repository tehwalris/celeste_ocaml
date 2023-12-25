function f(a, b)
  __print(a)
  __print(b)
end

function g(v)
  __print(v)
  return v
end

f(g(1))
f(g(2), g(3))
f(g(4), g(5), g(6))