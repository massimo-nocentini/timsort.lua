
local timsort = require 'timsort'

local tbl = {}
local n = math.tointeger(1e7)

local function coin (v)
	return math.random() < v
end

for i=1,n do
	local v
	if coin(0.7) then v = i else v = math.random(n) end
	table.insert(tbl, v)
end

--print(table.concat(tbl, ', ')..'\n')

local function C (a, b) return a < b end

local sorted, perm, elapsed = timsort.sort(tbl, C, false)

print(string.format('timsort.sort: %d seconds', elapsed))

local sorted_r, perm_r, elapsed_r = timsort.sort(tbl, C, true)

print(string.format('timsort.sort (reversed): %d seconds', elapsed_r))

local t_start = os.time()
table.sort(tbl, C)
local t_end = os.difftime(os.time(), t_start)

print(string.format('table.sort: %d seconds', t_end))

for i=1,n do
    assert(tbl[i] == sorted[i])
    assert(tbl[i] == sorted_r[n-i+1])
end

print('Same order checked.')

