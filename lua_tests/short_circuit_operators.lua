function f(s, v)
  __print(s)
  return v
end

__print(f("a", true) and f("b", true) and f("c", true))
__print(f("a", false) and f("b", true) and f("c", true))
__print(f("a", true) and f("b", false) and f("c", true))
__print(f("a", true) and f("b", true) and f("c", false))
__print(f("a", false) and f("b", false) and f("c", false))

__print(f("a", true) or f("b", true) or f("c", true))
__print(f("a", false) or f("b", true) or f("c", true))
__print(f("a", true) or f("b", false) or f("c", true))
__print(f("a", true) or f("b", true) or f("c", false))
__print(f("a", false) or f("b", false) or f("c", false))
__print(f("a", false) or f("b", false) or f("c", true))
__print(f("a", false) or f("b", true) or f("c", false))
__print(f("a", true) or f("b", false) or f("c", false))