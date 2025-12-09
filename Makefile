# CFLAGS=-std=c11 -g -fno-common
GHC=ghc

chibicc: *.hs
	$(GHC) -package mtl -package uniplate -o chibicc Main.hs

test: chibicc
	./test.sh

clean:
	rm -f chibicc *.o *.hi *~ tmp*

.PHONY: test clean
