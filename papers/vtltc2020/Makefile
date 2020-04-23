.PHONY: all clean

all: main.pdf presentation.pdf

main.pdf: references.bib
presentation.pdf: sosy-beamer.sty

%.pdf: %.tex
	pdflatex $<
	# rubber --pdf $<

clean:
	rm -f main.log main.aux
