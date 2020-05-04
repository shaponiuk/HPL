all:
	happy -gca ParHpl.y
	alex -g LexHpl.x
	ghc --make TestHpl.hs -o TestHpl

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocHpl.* LexHpl.* ParHpl.* LayoutHpl.* SkelHpl.* PrintHpl.* TestHpl.* AbsHpl.* TestHpl ErrM.* SharedString.* ComposOp.* Hpl.dtd XMLHpl.* Makefile*
	

