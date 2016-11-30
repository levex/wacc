all:
	stack build

install:
	stack install

clean:
	stack clean

.PHONY: all install clean
