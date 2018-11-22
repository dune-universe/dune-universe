beams := $(patsubst %.erl, %.beam, $(wildcard *.erl))

.PHONY: compile
compile: $(beams)

%.beam: %.erl
	erlc +debug_info -o $@ $<

.PHONY: clean
clean:
	rm -rf $(beams)
