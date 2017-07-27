help:
	@echo "Targets available:"
	@echo ""
	@echo "update-local   fetch new packages and delete old ones"
	@echo "update         update-local + commit and push"

update-local:
	./scripts/fetch.sh

update:
	./scripts/fetch.sh
	git add packages
	if git commit -m "Updated packages"; then git push; fi
