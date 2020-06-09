all:
	# stack install
	stack ghc -- src/Main.hs src/*/*.hs -o interpreter
	rm src/*.o src/*/*.o
	rm src/*.hi src/*/*.hi
