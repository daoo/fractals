build:
	@cabal-dev build --ghc-options="-H64m -rtsopts"

prof:
	@cabal-dev build --ghc-options="-rtsopts -prof -fprof-auto -H64m"

release:
	@cabal-dev build --ghc-options="-O2"

configure:
	@cabal-dev install --enable-library-profiling --enable-executable-profiling

ghci:
	@cabal-dev ghci

clean:
	@cabal-dev clean --save-configure

lint:
	hlint src

.PHONY: build clean ctags lint
