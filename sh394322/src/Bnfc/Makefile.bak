all:
	happy -gca ParHpl.y
	alex -g LexHpl.x
	latex DocHpl.tex; dvips DocHpl.dvi -o DocHpl.ps
	ghc --make TestHpl.hs -o TestHpl
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocHpl.ps
distclean: clean
	-rm -f DocHpl.* LexHpl.* ParHpl.* LayoutHpl.* SkelHpl.* PrintHpl.* TestHpl.* AbsHpl.* TestHpl ErrM.* SharedString.* Hpl.dtd XMLHpl.* Makefile*

