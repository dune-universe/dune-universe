.PHONY: all clean repl test

all:
	jbuilder build --dev

repl:
	jbuilder utop src -- -require ocaml-monadic

test:
	jbuilder runtest --dev

clean:
	jbuilder clean
