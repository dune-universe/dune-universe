beams := $(patsubst %.erl, %.beam, $(wildcard *.erl))

.PHONY: compile
compile: $(beams) otp_version

%.beam: %.erl
	erlc +debug_info -o $@ $<

otp_version:
	erl +V 2>&1 | awk '{ print $$NF }' | awk -F. '{ print $$1 }' | xargs expr 11 + | tr -d '\n' > otp_version

.PHONY: clean
clean:
	rm -rf $(beams) otp_version
