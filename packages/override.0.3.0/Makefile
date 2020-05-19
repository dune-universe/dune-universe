DUNE := dune
DUNE_PREFIX := _build/default
FLAGS =

examples_dir = examples
examples := $(notdir $(wildcard $(examples_dir)/*))

# All targets are phony targets since we want to rely on dune for
# dependency management.

.PHONY : all
all :
	$(DUNE) build $(FLAGS)

.PHONY : clean
clean :
	$(DUNE) clean $(FLAGS)

.PHONY : test
test :
	$(DUNE) runtest $(FLAGS)

.PHONY : examples
examples : $(examples)

.PHONY : install
install :
	$(DUNE) build @install $(FLAGS)
	$(DUNE) install $(FLAGS)

override.opam : dune-project
	$(DUNE) build override.opam $(FLAGS)

define foreach_example
.PHONY : $(example)
$(example) :
	$(DUNE) build $(examples_dir)/$(example)/$(example).exe $(FLAGS)
	$(DUNE_PREFIX)/$(examples_dir)/$(example)/$(example).exe
endef
$(foreach example,$(examples),$(eval $(foreach_example)))
