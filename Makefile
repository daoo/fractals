build:
	@cabal-dev build

release:
	@cabal-dev build --ghc-options="-O2"

ghci:
	@cabal-dev ghci

clean:
	@cabal clean
	@cabal-dev configure

ctags:
	echo ":ctags" | cabal-dev ghci -isrc/ -v0 src/Main.hs

lint:
	hlint src -c

.PHONY: build clean ctags lint
