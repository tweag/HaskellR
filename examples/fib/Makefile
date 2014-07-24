GHC=ghc
CFLAGS=-threaded -Wall -Werror
all:
	$(GHC) Fib.hs Main.hs $(CFLAGS) -o test

clean:
	-rm test Fib.hi Fib.o Fib.dyn_hi Fib.dyn_o Main.dyn_o Main.dyn_hi Main.hi Main.o
