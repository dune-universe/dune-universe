
BUILD = _build/default

# Main executable
MAIN = src/dryunit/main

# Tests
TEST_ALCOTEST = tests/alcotest/main
TEST_OUNIT = tests/ounit/main
TEST_ARGS = tests/extension/main


default:
	@jbuilder build $(MAIN).exe

clean:
	@rm -rf .dryunit
	@jbuilder clean

dryunit:
	@rm -f $(BUILD)/$(TEST_OUNIT).ml

run_alcotest:
	@jbuilder build $(TEST_ALCOTEST).exe && $(BUILD)/$(TEST_ALCOTEST).exe

run_ounit: dryunit
	@jbuilder build $(TEST_OUNIT).exe && $(BUILD)/$(TEST_OUNIT).exe

run_args: clean
	@jbuilder build $(TEST_ARGS).exe && $(BUILD)/$(TEST_ARGS).exe

build_args: clean
	@jbuilder build $(EXE_ARGS)

test: clean default
	@jbuilder runtest

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall install

run: default
	@$(BUILD)/$(MAIN).exe $(filter-out $@,$(MAKECMDGOALS))

%:
	@:

.PHONY: default install uninstall reinstall clean examples
