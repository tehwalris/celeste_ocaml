-- TODO test against pico8

function make_example_table()
  local x = {}
  add(x, 3)
  add(x, 1)
  add(x, 'walrus')
  return x
end

function test_normal_foreach()
  local x = make_example_table()

  local y = {}
  foreach(x, function(v)
    add(y, v)
  end)

  __assert(#y == 3)
  __assert(y[1] == 3)
  __assert(y[2] == 1)
  __assert(y[3] == 'walrus')
end

function test_foreach_with_modification()
  local x = make_example_table()

  local y = {}
  foreach(x, function(v)
    if v == 3 then
      x[2] = 2
    end
    add(y, v)
  end)

  __assert(#y == 3)
  __assert(y[1] == 3)
  __assert(y[2] == 2)
  __assert(y[3] == 'walrus')
end

function test_foreach_with_addition()
  local x = make_example_table()

  local y = {}
  foreach(x, function(v)
    if v == 1 then
      add(x, 4)
    elseif v == 4 then
      add(x, 'WALRUS')
    end
    add(y, v)
  end)

  __assert(#y == 5)
  __assert(y[1] == 3)
  __assert(y[2] == 1)
  __assert(y[3] == 'walrus')
  __assert(y[4] == 4)
  __assert(y[5] == 'WALRUS')
end


foreach(make_example_table(), __print)
test_normal_foreach()
test_foreach_with_modification()
test_foreach_with_addition()