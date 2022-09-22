# timsortlua

This repository contains a `timsort` Lua module that provides a `sort` function as an auxiliary sorting algorithm based on https://en.wikipedia.org/wiki/Timsort and, more in particular, on [listsort.txt](https://github.com/python/cpython/blob/b4f5f07d076d7e7d825306981108dbb7120d7377/Objects/listsort.txt) (which we copy under the root directory).

## libtimsort

The implementation is totally based and strictly follows the Python one, namely https://github.com/python/cpython/blob/b4f5f07d076d7e7d825306981108dbb7120d7377/Objects/listobject.c#L2251. For the sake of clarity, we copy the necessary functions and type definitions from the Python's C implementation and remove some details to make it compile to have a Lua module.

## Usage

Consider the script `src/simple.lua` that reads as follows:
```lua
local timsort = require 'timsort'

local tbl = {}
local n = 20

for i=1,n do
    table.insert(tbl, math.random(n))
end

print(table.concat(tbl, ', '))

local sorted = timsort.sort(tbl)

print(table.concat(sorted, ', '))
```
and running it yields:
```bash
$ lua simple.lua 
10, 9, 17, 19, 20, 16, 5, 4, 19, 9, 13, 8, 17, 9, 3, 3, 19, 18, 1, 12
1, 3, 3, 4, 5, 8, 9, 9, 9, 10, 12, 13, 16, 17, 17, 18, 19, 19, 19, 20
```
