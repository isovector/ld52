ld52.cabal: package.yaml
	hpack

run: ld52.cabal
	cabal run
.PHONY: run

repl: ld52.cabal
	cabal repl
.PHONY: repl
