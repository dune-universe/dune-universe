
ifndef LUTIN
  LUTIN=../../_build/lutin/src/main.native
endif


go: 
	luciole-rif $(LUTIN) -l 1   env.lut -m main || 	[ -s rdbg.rif ]



test: go
	rm -f luciole.rif
