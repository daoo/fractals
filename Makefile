build:
	@cabal build --ghc-options="-Wall"

mandelbrot:
	@cabal clean
	@cabal configure --disable-library-profiling --disable-executable-profiling
	@cabal build --ghc-options="-Wall -O2 -fignore-asserts -DMANDELBROT" fractals-image
	@./dist/build/fractals-image/fractals-image

llvm:
	@cabal clean
	@cabal configure --disable-library-profiling --disable-executable-profiling
	@cabal build --ghc-options="-Wall -O2 -fignore-asserts -fllvm -optlo-O3 -optlo-march=native -optlo-mattr=native"

doc:
	@cabal haddock

test:
	@cabal configure --enable-tests
	@cabal build --ghc-options="-Wall" fractals-doctest
	@./dist/build/fractals-doctest/fractals-doctest

clean:
	@cabal clean

lint:
	@hlint src
