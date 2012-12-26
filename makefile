all: report bin

bin:
	cd src && $(MAKE)

report:	srcreport
	cd report && $(MAKE)
	cd report && $(MAKE) bib
	cd report && $(MAKE)

srcreport: src/reactive-jabber.lhs src/Network/Parser.lhs src/Network/XMPPTypes.lhs src/Network/XMPPMapping.lhs src/Network/XMPP.lhs
	lhs2TeX src/reactive-jabber.lhs -o report/reactive-jabber.tex
	lhs2TeX src/Network/Parser.lhs -o report/Parser.tex
	lhs2TeX src/Network/XMPPTypes.lhs -o report/XMPPTypes.tex
	lhs2TeX src/Network/XMPPMapping.lhs -o report/XMPPMapping.tex
	lhs2TeX src/Network/XMPP.lhs -o report/XMPP.tex

clean:
	rm -f *~
	rm -f src/reactive-jabber
	rm -f src/*~
	rm -f src/*.hi
	rm -f src/*.o
	rm -f src/Network/*~
	rm -f src/Network/*.hi
	rm -f src/Network/*.o
	cd report && $(MAKE) clean
