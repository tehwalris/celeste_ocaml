x = {}
len_x = 100
for i = 1, len_x do
  x[i] = i
end

for rep = 1, 50 do
  __debug("rep", rep)
  local sum = 0
  for i = 1, len_x do
    sum = sum + x[i]
  end
end