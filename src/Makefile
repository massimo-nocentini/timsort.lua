
linux:
	gcc -O2 -std=gnu99 -fPIC -Wall -I/usr/local/include/ -s -shared -o libluatimsort.so libluatimsort.c -L/usr/local/lib/ -llua -ltimsort

macos:
	gcc -O2 -std=gnu99 -fPIC -Wall -I/usr/local/include/ -dynamiclib -o libluatimsort.so libluatimsort.c -L/usr/local/lib/ -llua -ltimsort

install:
	mkdir -p /usr/local/lib/lua/5.4/	# just for ensuring that the dest dir exists
	mkdir -p /usr/local/share/lua/5.4/	# just for ensuring that the dest dir exists
	mv libluatimsort.so /usr/local/lib/lua/5.4/
	cp timsort.lua /usr/local/share/lua/5.4/
