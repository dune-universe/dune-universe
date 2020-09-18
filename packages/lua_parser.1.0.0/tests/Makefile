.PHONY: build clean

build:
	luajit trans.lua
	cp ./Makefile.test ./fout/Makefile
	$(MAKE) -C fout

clean:
	rm -fr fout l2l

