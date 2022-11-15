
/* 
	This is a glue c file for importing delta client c functions into Lua workflow.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <lua.h>
#include <lauxlib.h>
#include <time.h>
#include <timsort.h>

typedef struct userdata_s {
    lua_State *L;
    lua_Integer table_absidx;
} userdata_t;

int cmp(item_t v, item_t w, mergestate_t *ms)
{
    userdata_t *ud = (userdata_t *) ms->userdata;

    int table_absidx = ud->table_absidx;

    lua_State *L = ud->L;
    lua_pushvalue(L, table_absidx + 2);
    lua_geti(L, table_absidx, v);
    lua_geti(L, table_absidx, w);

    lua_call(L, 2, 1);

    int lt = lua_toboolean(L, -1);

    lua_pop(L, 1);

    return lt;
}

static int l_sort(lua_State *L) {

    //int nargs = lua_gettop(L);

	/* Initial checks */
	assert(lua_istable(L, -3));
	assert(lua_isboolean(L, -2));
    assert(lua_isfunction(L, -1));

    int table_absidx = lua_absindex(L, -3);

    userdata_t ud;
    ud.L = L;
    ud.table_absidx = table_absidx;

    lua_len(L, table_absidx);  // push on the stack the number of elements to sort.
    int nel = lua_tointeger(L, -1); // get that number.
    lua_pop(L, 1);                  // clean the stack.

    int reverse = lua_toboolean(L, -2);

    time_t starttime = time(NULL);
    sortslice_t perm = timsort (nel, reverse, cmp, &ud);
    time_t endtime = time(NULL);

    if (perm == NULL) {
        lua_pushstring(L, "timsort error");
        lua_error(L);
    }

    lua_createtable(L, nel, 0);	// the sorted table
    lua_createtable(L, nel, 0);	// the sorting permutation

    for (int i = 0; i < nel; i++) {
        int idx = perm[i];

        lua_geti(L, table_absidx, idx);
        lua_seti(L, -3, i + 1);

	    lua_pushinteger(L, idx);	// to also provide the sorting permutation.
        lua_seti(L, -2, i + 1);
    }
    lua_pushinteger(L, endtime - starttime);

    return 3;
}

static const struct luaL_Reg timsort_reg [] = {
	{"sort", l_sort},
	{NULL, NULL} /* sentinel */
};
 
int luaopen_libluatimsort (lua_State *L) {
	luaL_newlib(L, timsort_reg);
	return 1;
}
