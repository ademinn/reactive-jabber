all: Main.hs
	ghc --make Main.hs

clean:
	rm -f *~
	rm -f Main
	rm -f *.hi
	rm -f *.o
