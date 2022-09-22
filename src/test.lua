
local timsort = require 'timsort'

local tbl = {}
local n = 100

for i=1,n do
    table.insert(tbl, math.random(n))
end

print(table.concat(tbl, ', '))

local function C (a, b)

    print('Comparing ' .. tostring(a) .. ' with ' .. tostring(b))
    return a < b

end

local sorted = timsort.sort(tbl, false, C)

--[[
local sorted = {}
for i, v in ipairs(tbl_perm) do
    sorted[i] = tbl[v]
end
--]]

print(table.concat(sorted, ', '))

