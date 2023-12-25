x = {}
for i = 1, 100 do
  x[i] = i
end

for rep = 1, 50 do
  __debug("rep", rep)
  local sum = 0
  foreach(x, function(v)
    sum = sum + v
  end)
end