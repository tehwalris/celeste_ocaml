function f(a, b)
  print(a)
  print(b)
end

function g(v)
  print(v)
  return v
end

f(g(1))
f(g(2), g(3))
f(g(4), g(5), g(6))