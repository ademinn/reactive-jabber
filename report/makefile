all:
	lhs2TeX lhspreamble.lhs -o lhspreamble.tex
	pdflatex main.tex

bib:
	bibtex main.aux

clean:
	rm -f *~
	rm -f main.pdf
	rm -f *.aux
	rm -f *.ptb
	rm -f *.toc
	rm -f *.log
	rm -f *.bbl
	rm -f *.blg
	rm -f *.out
	rm -f lhspreamble.tex
	rm -f reactive-jabber.tex
	rm -f Parser.tex
	rm -f XMPP.tex
	rm -f XMPPTypes.tex
	rm -f XMPPMapping.tex
