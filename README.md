# timsort.lua

This repository contains a `timsort` Lua module that provides a `sort` function as an auxiliary sorting algorithm based on https://en.wikipedia.org/wiki/Timsort and, more in particular, on [listsort.txt](https://github.com/python/cpython/blob/master/Objects/listsort.txt) (which we copy under the root directory).

It is interesting to point out the reading of http://www.envisage-project.eu/proving-android-java-and-python-sorting-algorithm-is-broken-and-how-to-fix-it/ found in the [README.md](https://github.com/tvanslyke/timsort-cpp#readme) of @tvanslyke.

## Under the hood

This repository has two separate folder both for sources and lua test scripts, each described in the following sections. 

### [`src/libtimsort.c`](https://github.com/massimo-nocentini/timsort.lua/blob/master/src/libtimsort.c)

The implementation is *totally based* and strictly follows the Python one, namely https://github.com/python/cpython/blob/master/Objects/listobject.c#L2251. For the sake of clarity, we copy the necessary functions and type definitions from the Python's C implementation and remove some details to make it compile as a Lua module. We do not do **any** change to the original algorithm; after all, consider this project as a way to distill the Tim's sorting core functions from the Python C file about `list` objects (in fact, many names in the implementation maintain the prefix `Py`).

### [`src/timsort.lua`](https://github.com/massimo-nocentini/timsort.lua/blob/master/src/timsort.lua)

The higher level interface over the C implementation. It exports the function `timsort.sort` such that the expression
```lua
local sortedtbl = timsort.sort (tbl, C, reverse)
```
means that `sortedtbl` contains the same elements contained in `tbl` but in a different order according to the comparison function `C`, possibly reversed if `reverse` is `true`. By the way, `tbl` is never modified and if `C` is not given then `<` is used for it; finally, if `reverse` is not given then `false` is used for it.

## Usage

### Compilation

It is mandatory to compile by typing `make`, no more deps that default unix build tools.

### Testing

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
