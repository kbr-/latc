all: ParLatte.hs LexLatte.hs AbsLatte.hs ErrM.hs

ParLatte.y LexLatte.x AbsLatte.hs ErrM.hs: Latte.cf
	bnfc $<

ParLatte.hs: ParLatte.y
	happy -gca $<

LexLatte.hs: LexLatte.x
	alex -g LexLatte.x

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocLatte.ps

distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* Latte.dtd XMLLatte.* info.txt
