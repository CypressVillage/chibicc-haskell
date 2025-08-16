# CFLAGS=-std=c11 -g -fno-common
GHC=ghc

chibicc: *.hs
	$(GHC) -o chibicc Main.hs

test: chibicc
	./test.sh

clean:
	rm -f chibicc *.o *.hi *~ tmp*

.PHONY: test clean
