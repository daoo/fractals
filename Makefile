disable_all=--disable-library-profiling --disable-executable-profiling --disable-tests --disable-benchmarks

build:
	@cabal build --ghc-options="-Wall -O -rtsopts -fno-ignore-asserts"

check:
	@cabal build --ghc-options="-Wall -c" fractals
	@cabal build --ghc-options="-Wall -c" fractals-image

prof:
	@cabal configure --enable-library-profiling --enable-executable-profiling
	@cabal build --ghc-options="-Wall -rtsopts -prof -fprof-auto"

optimal:
	@cabal configure $(disable_all)
	@cabal build --ghc-options="-Wall -O2 -fignore-asserts"

llvm:
	@cabal configure $(disable_all)
	@cabal build --ghc-options="-Wall -O2 -fignore-asserts -fllvm -optlo-O3 -optlo-march=native -optlo-mattr=native"

test:
	@cabal test

clean:
	@cabal clean --save-configure
	@cabal configure $(disable_all)

lint:
	@hlint src
