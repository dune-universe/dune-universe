MAIN  := Test
BUILD := ../_build/default/demo

.PHONY: all
all:
	dune build $(MAIN).exe

.PHONY: test
test: all
	$(BUILD)/$(MAIN).exe

.PHONY: clean
clean:
	rm -f *~
	dune clean
