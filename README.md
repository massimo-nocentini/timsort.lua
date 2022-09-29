# timsort.lua

This repository contains a `timsort` Lua module that provides a `sort` function as an auxiliary sorting algorithm based on https://en.wikipedia.org/wiki/Timsort and, more in particular, on [listsort.txt](https://github.com/python/cpython/blob/master/Objects/listsort.txt).

For what concerns computational times, it is interesting to point out the reading of http://www.envisage-project.eu/proving-android-java-and-python-sorting-algorithm-is-broken-and-how-to-fix-it/ found in the [README.md](https://github.com/tvanslyke/timsort-cpp#readme) of [@tvanslyke](https://github.com/tvanslyke).

## Under the hood

This repository has two separate folder both for sources and lua test scripts, each described in the following sections. 

### [`src/libtimsort.c`](https://github.com/massimo-nocentini/timsort.lua/blob/master/src/libtimsort.c)

The implementation is *totally based* and strictly follows the Python one, namely https://github.com/python/cpython/blob/master/Objects/listobject.c#L2251. For the sake of clarity, we copy the necessary functions and type definitions from the Python's C implementation and remove some details to make it compile as a Lua module. We do not do **any** change to the original algorithm. On one hand, we just renamed and defined `typedef`s to make it readable outside of the Python context; On the other hand, let consider this project as a way to distill the Tim's sorting core functions from the Python C file about `list` objects.

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

local sorted, perm = timsort.sort(tbl)

print(table.concat(sorted, ', '))
print(table.concat(perm, ', '))
```
and running it yields:
```bash
$ lua simple.lua
2, 3, 18, 16, 13, 20, 2, 2, 2, 1, 6, 7, 7, 8, 2, 11, 8, 3, 17, 20
1, 2, 2, 2, 2, 2, 3, 3, 6, 7, 7, 8, 8, 11, 13, 16, 17, 18, 20, 20
10, 1, 7, 8, 9, 15, 2, 18, 11, 12, 13, 14, 17, 16, 5, 4, 19, 3, 6, 20

```

A more involving examples can be found in `src/linear.lua` where a huge array is populated with integers in increasing order according to a biased coin, in case of tail just sample a random number. Such examples produces the following output,
```lua
$ lua linear.lua 
timsort.sort: 9 seconds
timsort.sort (reversed): 10 seconds
table.sort: 17 seconds
Same order checked.
```
showing the performance gain with respect to `table.sort` in the presence of *runs*.
