disable_all=--disable-library-profiling --disable-executable-profiling --disable-tests --disable-benchmarks

check:
	@cabal build --ghc-options="-Wall -c" fractals
	@cabal build --ghc-options="-Wall -c" fractals-image

all:
	@cabal build --ghc-options="-Wall -O -rtsopts -fno-ignore-asserts"

prof:
	@cabal configure --enable-library-profiling --enable-executable-profiling
	@cabal build --ghc-options="-Wall -rtsopts -prof -fprof-auto"

release:
	@cabal clean
	@cabal configure $(disable_all)
	@cabal build --ghc-options="-Wall -O2 -fignore-asserts"

init:
	@cabal sandbox init
	@cabal install --only-dependencies --enable-library-profiling
	@cabal configure $(disable_all)

test:
	@cabal configure --enable-tests
	@cabal build --ghc-options="-Wall -O -fno-ignore-asserts"
	./dist/build/tests/tests

clean:
	@cabal clean --save-configure
	@cabal configure $(disable_all)

lint:
	@hlint src
