ld52.cabal: package.yaml
	test $$IN_NIX_SHELL
	hpack

run: ld52.cabal
	test $$IN_NIX_SHELL
	cabal run
.PHONY: run

repl: ld52.cabal
	test $$IN_NIX_SHELL
	cabal repl
.PHONY: repl
