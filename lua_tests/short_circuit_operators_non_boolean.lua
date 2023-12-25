function f(s, v)
  print(s)
  return v
end

print(f("a", true) and f("b", -4) or f("c", 0))
print(f("a", false) and f("b", -4) or f("c", 0))
print(f("a", 3) and f("b", -4) or f("c", 0))
print(f("a", 0) and f("b", -4) or f("c", 0))
print(f("a", 0) and f("b", 0) or f("c", 4))

print(f("a", -3) and f("b", 0))
print(f("a", 0) and f("b", 0))
print(f("a", 3) and f("b", 0))
print(f("a", 0.3) and f("b", 0))
print(f("a", "") and f("b", 0))
print(f("a", " ") and f("b", 0))
print(f("a", "a") and f("b", 0))
print(f("a", nil) and f("b", 0))
print(f("a", {}) and f("b", 0))
print(f("a", f) and f("b", 0))