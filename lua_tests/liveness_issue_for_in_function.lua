function function_with_for()
  for i = 1, 7 do
  end
end

local y = {}
function_with_for()
__print(#y)

-- The interpreter used to crash at "print" because the function call was
-- causing the outer locals to be lost. This only happened with functions that
-- have multiple cfg blocks.