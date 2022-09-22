
local libtimsort = require 'libtimsort'

local timsort = {}

setmetatable(timsort, { __index = libtimsort })

function timsort.sort (tbl, reverse, C)

    reverse = reverse or false
    C = C or function (a, b) return a < b end
    return libtimsort.sort(tbl, reverse, C)

end

return timsort