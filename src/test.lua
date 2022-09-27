
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

local sorted, elapsed = timsort.sort(tbl, C, false)

print(string.format('timsort.sort: %d seconds', elapsed))

local reversed, elapsed_r = timsort.sort(tbl, C, true)

print(string.format('timsort.sort (reversed): %d seconds', elapsed_r))


local t_start = os.time()
table.sort(tbl, C)
local t_end = os.difftime(os.time(), t_start)

print(string.format('table.sort: %d seconds', t_end))

for i=1,n do
    assert(tbl[i] == sorted[i])
    assert(tbl[i] == reversed[n-i+1])
end

print('Same order checked.')

