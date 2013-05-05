build:
	@cabal-dev build --ghc-options="-H64m"

prof:
	@cabal-dev build --ghc-options="-rtsopts -prof -fprof-auto -H64m"

release:
	@cabal-dev build --ghc-options="-O2"

ghci:
	@cabal-dev ghci

clean:
	@cabal-dev clean --save-configure

lint:
	hlint src

.PHONY: build clean ctags lint
