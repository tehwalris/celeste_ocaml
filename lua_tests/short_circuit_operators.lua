function f(s, v)
  print(s)
  return v
end

print(f("a", true) and f("b", true) and f("c", true))
print(f("a", false) and f("b", true) and f("c", true))
print(f("a", true) and f("b", false) and f("c", true))
print(f("a", true) and f("b", true) and f("c", false))
print(f("a", false) and f("b", false) and f("c", false))

print(f("a", true) or f("b", true) or f("c", true))
print(f("a", false) or f("b", true) or f("c", true))
print(f("a", true) or f("b", false) or f("c", true))
print(f("a", true) or f("b", true) or f("c", false))
print(f("a", false) or f("b", false) or f("c", false))
print(f("a", false) or f("b", false) or f("c", true))
print(f("a", false) or f("b", true) or f("c", false))
print(f("a", true) or f("b", false) or f("c", false))