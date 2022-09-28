
linux:
	gcc -O2 -std=gnu99 -fPIC -Wall -g -I/usr/local/include/ -s -shared -o \
		libtimsort.so libtimsort.c -L/usr/local/lib/ -llua 

install:
	mkdir -p /usr/local/lib/lua/5.4/	# just for ensuring that the dest dir exists
	mkdir -p /usr/local/share/lua/5.4/	# just for ensuring that the dest dir exists
	mv libtimsort.so /usr/local/lib/lua/5.4/
	cp timsort.lua /usr/local/share/lua/5.4/
