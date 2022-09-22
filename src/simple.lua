
local timsort = require 'timsort'

local tbl = {}
local n = 20

for i=1,n do
    table.insert(tbl, math.random(n))
end

print(table.concat(tbl, ', '))

local sorted = timsort.sort(tbl)

print(table.concat(sorted, ', '))

