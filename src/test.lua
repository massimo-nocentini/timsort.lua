
local timsort = require 'timsort'

local tbl = {}
local n = math.tointeger(1e7)

for i=1,n do
    table.insert(tbl, math.random(n))
end

--print(table.concat(tbl, ', ')..'\n')

local function C (a, b)
    --print('Comparing ' .. tostring(a) .. ' with ' .. tostring(b))
    return a < b
end

local sorted, elapsed = timsort.sort(tbl, false, C)

print(string.format('timsort.sort: %d seconds', elapsed))


local t_start = os.time()
table.sort(tbl, C)
local t_end = os.difftime(os.time(), t_start)

print(string.format('table.sort: %d seconds', t_end))

for i=1,n do
    assert(tbl[i] == sorted[i])
end

print('Same order checked.')

