test-drom-new:
	rm -rf drom-test
	../drom new drom-test
	cd drom-test; \
		git commit -m "Import initial files by 'drom new'" && \
		git push --set-upstream origin master -f
	cd drom-test; \
		make html && \
		git add docs && \
		git commit -m "Import documentation'" && \
		git push

