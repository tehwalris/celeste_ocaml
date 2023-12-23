if __new_unknown_boolean() then
  x = {}
  x[0] = "a"
  y = {}
  y[0] = "b"
else
  y = {}
  y[0] = "b"
  x = {}
  x[0] = "a"
end
print(x[0])
print(y[0])