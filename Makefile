
build:
	@cabal build --ghc-options="-Wall -O -fno-ignore-asserts"

check:
	@cabal build --ghc-options="-Wall -c" fractals
	@cabal build --ghc-options="-Wall -c" fractals-image

prof:
	@cabal clean
	@cabal configure --enable-library-profiling --enable-executable-profiling
	@cabal build --ghc-options="-Wall -prof -fprof-auto"

optimal:
	@cabal clean
	@cabal build --ghc-options="-Wall -O2 -fignore-asserts"

mandelbrot:
	@cabal clean
	@cabal build --ghc-options="-Wall -O2 -fignore-asserts -DMANDELBROT" fractals-image fractals-ascii

llvm:
	@cabal clean
	@cabal build --ghc-options="-Wall -O2 -fignore-asserts -fllvm -optlo-O3 -optlo-march=native -optlo-mattr=native"

doc:
	@cabal haddock

test:
	@cabal configure --enable-tests
	@cabal build fractals-doctest --ghc-options="-Wall"
	@./dist/build/fractals-doctest/fractals-doctest

clean:
	@cabal clean

lint:
	@hlint src
