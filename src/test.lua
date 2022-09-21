
local timsort = require 'timsort'

local tbl = {}
local n = 100

for i=1,n do
    table.insert(tbl, math.random(n))
end

print(table.concat(tbl, ', '))

local sorted = timsort.sort(tbl, false, function (a, b) return a < b end)

--[[
local sorted = {}
for i, v in ipairs(tbl_perm) do
    sorted[i] = tbl[v]
end
--]]

print(table.concat(sorted, ', '))

