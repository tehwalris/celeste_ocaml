function f(s, v)
  __print(s)
  return v
end

__print(f("a", true) and f("b", -4) or f("c", 0))
__print(f("a", false) and f("b", -4) or f("c", 0))
__print(f("a", 3) and f("b", -4) or f("c", 0))
__print(f("a", 0) and f("b", -4) or f("c", 0))
__print(f("a", 0) and f("b", 0) or f("c", 4))

__print(f("a", -3) and f("b", 0))
__print(f("a", 0) and f("b", 0))
__print(f("a", 3) and f("b", 0))
__print(f("a", 0.3) and f("b", 0))
__print(f("a", "") and f("b", 0))
__print(f("a", " ") and f("b", 0))
__print(f("a", "a") and f("b", 0))
__print(f("a", nil) and f("b", 0))
__print(f("a", {}) and f("b", 0))
__print(f("a", f) and f("b", 0))