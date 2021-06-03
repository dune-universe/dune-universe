CC = gcc
CFLAGS = -fpic

all: libradamsa.c radamsa.h
	$(CC) -fPIC -I $(PWD) -o libradamsa.a -c libradamsa.c
	$(CC) -fPIC -shared -I $(PWD) -o libradamsa.so libradamsa.c

gen: gen.c
	cc -O2 -I $(PWD) -o testcase-gen gen.c libradamsa.a 

clean:
	rm -f libradamsa.a libradamsa.so
