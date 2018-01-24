default: repl

all: clean repl

repl:
	jbuilder build

clean:
	jbuilder clean

test: repl
	@_build/default/bin/mnml.exe tests/core.l
	@_build/default/bin/mnml.exe tests/list.l

