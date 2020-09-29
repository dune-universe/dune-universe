all:
	dune build --profile release

test: all
	cp -f _build/default/test/test_vue.bc.js test/test-vue.js
	cp -f _build/default/test/test_router.bc.js test/test-router.js
	cp -f _build/default/test/test_chart.bc.js test/test-chart.js
	cp -f _build/default/test/test_vuex.bc.js test/test-vuex.js
	cp -f _build/default/test/test_vuex2.bc.js test/test-vuex2.js
	cp -f _build/default/test/test_project.bc.js test/test-project.js

clean:
	dune clean

install:
	dune install

doc:
	dune build @doc
	cp -rf _build/default/_doc/_html/* docs
