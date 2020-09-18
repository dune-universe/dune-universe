.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest --force

.PHONY: clean
clean:
	dune clean

_build/fixed-release/%.tbz: _build/%.tbz
	mkdir -p $(dir $@)
	(cd $(dir $@) && tar -xjf ../$(notdir $<))
	cp -r uap-core/* $(dir $@)/$(notdir $(basename $<))/uap-core
	(cd $(dir $@) && tar -cjf $(notdir $<) $(notdir $(basename $<)))
	$(RM) -r $(dir $@)/$(notdir $(basename $<))

# we are using dune-release to do the release, but dune-release doesn't want to
# include the files from uap-core, so we manually have to append those to the
# tar archive
.PHONY: release
release: clean build test
	$(RM) _build/user-agent-parser-*.tbz
	dune-release tag
	dune-release distrib --skip-build
	$(MAKE) _build/fixed-release/`cd _build/ && ls *.tbz | head -n1`
	dune-release publish --dist-file `ls _build/fixed-release/*.tbz`
	dune-release opam pkg --dist-file `ls _build/fixed-release/*.tbz`
