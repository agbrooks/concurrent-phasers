all:
	stack build

clean:
	stack clean

test:
	stack test

doc:
	stack haddock
