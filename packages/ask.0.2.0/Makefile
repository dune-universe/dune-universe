.PHONY: all
all:
	opam exec -- dune build --root . @install

.PHONY: lock
lock: ## Generate a lock file
	opam lock -y .

.PHONY: build
build: ## Build the project
	opam exec -- dune build

.PHONY: watch
watch: ## Build and watch the project
	opam exec -- dune build --watch

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean

.PHONY: test test-all
test:
	SIHL_ENV=test ./_build/default/ask/test/ask_mariadb.exe
	SIHL_ENV=test ./_build/default/ask-integrator/test/ask_integrator_mariadb.exe

.PHONY: test-ask
test-ask:
	SIHL_ENV=test ./_build/default/ask/test/ask_mariadb.exe

.PHONY: test-integrator
test-integrator:
	SIHL_ENV=test ./_build/default/ask-integrator/test/ask_integrator_mariadb.exe
