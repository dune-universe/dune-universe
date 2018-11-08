ALL = examples/summary/writer.exe

%.exe: .FORCE
	dune build $@

all: .FORCE
	dune build $(ALL)

clean:
	rm -Rf _build/ *.exe

.FORCE:
