all:
	cabal update
	cabal install --dependencies-only
	cabal configure
	cabal install

clean:
	cabal clean

.PHONY: all clean
