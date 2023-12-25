function __reset_button_states()
  __button_states = {}
  for i = 1,6 do
    __button_states[i] = __new_unknown_boolean()
  end
end

function btn(i)
  -- TODO What happens if this is called during _draw? Are the values guaranteed
  -- to be consistent with _update?

  __assert(i >= 0)
  __assert(i <= 5)
  i = i + 1
  -- This weird if statement concretizes the button
  -- state the first time it is read.
  if __button_states[i] then
    __button_states[i] = true
    return true
  else
    __button_states[i] = false
    return false
  end
end

-- Noop functions

function music() end
function sfx() end
function pal() end
function rectfill() end
function map() end