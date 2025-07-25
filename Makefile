# CFLAGS=-std=c11 -g -fno-common
GHC=ghc

chibicc: main.hs
	$(GHC) -o chibicc *.hs

test: chibicc
	./test.sh

clean:
	rm -f chibicc *.o *.hi *~ tmp*

.PHONY: test clean
