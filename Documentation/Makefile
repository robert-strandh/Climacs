all: climacs-internals.pdf climacs-user.pdf

%.eps: %.fig
	fig2dev -Leps -m 0.75  $< $@

climacs-internals.pdf: climacs-internals.texi undo.eps
	texi2pdf climacs-internals.texi

climacs-user.pdf: climacs-user.texi
	texi2pdf climacs-user.texi

clean:
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pdf *.pg
	rm -f *.toc *.tp *.tps *.vr *.dvi *.ps *.eps *.kys *.cps *~
