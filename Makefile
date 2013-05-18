build:
	@cabal-dev build --ghc-options="-H64m -rtsopts"

prof:
	@cabal-dev build --ghc-options="-rtsopts -prof -fprof-auto -H64m"

release:
	@cabal-dev build --ghc-options="-fllvm -H64m -O2"

configure:
	@cabal-dev install \
		--reinstall \
		--force-reinstalls \
		--enable-benchmarks \
		--enable-tests
	@cabal-dev configure \
		--enable-benchmarks \
		--enable-tests

ghci:
	@cabal-dev ghci

clean:
	@cabal-dev clean --save-configure

lint:
	@hlint src
