.PHONY: all
all:
	dune build @all

.PHONY: clean
clean:
	dune clean

.PHONY: test
test:
	dune runtest

.PHONY: doc
doc:
	dune build @doc

DOC_PATH := _build/default/_doc/_html
INSTALL_DOC_PATH := docs/doc

.PHONY: doc-browse
doc-browse: doc
	xdg-open ${DOC_PATH}/index.html

.PHONY: doc-commit
doc-commit: doc
# compute the current commit hash and branch name
	@git rev-parse --short HEAD > /tmp/commit
	@git rev-parse --abbrev-ref HEAD > /tmp/branch
	@git config branch.pages.remote > /tmp/remote-upstream-for-pages

# move to the 'pages' branch and commit the documentation there
	git checkout pages
	mkdir -p ${INSTALL_DOC_PATH}
	@git rm --ignore-unmatch -r ${INSTALL_DOC_PATH} > /dev/null
	cp -r ${DOC_PATH}/ ${INSTALL_DOC_PATH}/
	@git add ${INSTALL_DOC_PATH}
	@git commit -m "documentation for $$(cat /tmp/branch) ($$(cat /tmp/commit))" \
	&& git checkout $$(cat /tmp/branch) \
	|| git checkout $$(cat /tmp/branch)

	@echo
	@echo "The documentation was committed to the 'pages' branch."
	@echo "You can now push it with"
	@echo "    git push $$(cat /tmp/remote-upstream-for-pages) pages"
