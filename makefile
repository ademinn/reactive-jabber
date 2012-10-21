all: Main.hs
	ghc --make -threaded Main.hs

clean:
	rm -f *~
	rm -f Main
	rm -f *.hi
	rm -f *.o
	rm -f Network/*~
	rm -f Network/*.hi
	rm -f Network/*.o
