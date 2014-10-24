build:
	@cabal build --ghc-options="-Wall"

mandelbrot:
	@cabal clean
	@cabal build --ghc-options="-Wall -O2 -fignore-asserts -DMANDELBROT" fractals-image

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
