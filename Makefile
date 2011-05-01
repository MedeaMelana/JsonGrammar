default: run

run:
	ghci -Wall Domain

clean:
	cabal clean

configure:
	cabal configure

docs: configure
	cabal haddock

install:
	cabal install

opendocs: docs
	open dist/doc/html/JsonGrammar/index.html
