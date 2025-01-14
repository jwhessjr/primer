# NOTE:
#
# Most commands assume you're running this from the top-level `nix
# develop` shell.

targets = build configure check test bench generate-fixtures docs clean realclean deps

$(targets):
	$(MAKE) -C primer $@
	$(MAKE) -C primer-rel8 $@
	$(MAKE) -C primer-selda $@
	$(MAKE) -C primer-service $@
	$(MAKE) -C primer-benchmark $@

weeder:
	cabal build all --enable-benchmarks --enable-tests
	weeder
	@echo "No issues found."

openapi.json: build
	cabal run -v0 primer-service:exe:primer-openapi > $@
	openapi-generator-cli validate --recommend -i $@

.PHONY: $(targets) weeder
