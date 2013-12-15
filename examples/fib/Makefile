GHC=ghc
CFLAGS=-threaded -Wall -Werror
all:
	$(GHC) Fib.hs Main.hs $(CFLAGS) -o test

clean:
	rm test Fib.hi Fib.o Main.hi Main.o
