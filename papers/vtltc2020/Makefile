.PHONY: clean

main.pdf: main.tex definitions.tex
	pdflatex --shell-escape $<

clean:
	rm -f main.log main.aux
