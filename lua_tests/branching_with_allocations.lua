if __new_unknown_boolean() then
  x = {}
  x[1] = "a"
  y = {}
  y[1] = "b"
else
  y = {}
  y[1] = "b"
  x = {}
  x[1] = "a"
end
__print(x[1])
__print(y[1])