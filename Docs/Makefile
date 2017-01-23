### Makefile for compiling .tex files ###

#All targets
DOCS = $(patsubst %.tex,%.pdf,$(wildcard *.tex))
IMGS = $(patsubst %.svg,%.pdf,$(wildcard resources/*.svg))

#Fail silently
MAKEFLAGS = --silent

docs: $(IMGS) $(DOCS) clean-tex

docs-dbg: $(IMGS) $(DOCS)

no-imgs: $(DOCS) clean-tex

resources/%.pdf: resources/%.svg
	inkscape -D -z --file=$^ --export-pdf=$@

%.pdf: %.tex
	- pdflatex $^
	- bibtex $*
	- pdflatex $^
	- pdflatex $^

#Remove all temporary files created by pdfLaTeX
clean-tex:
	- rm *.aux *.bbl *.blg *.log 2> /dev/null

clean-imgs:
	- rm resources/*.pdf 2> /dev/null

clean: clean-tex clean-imgs
	- rm *.pdf 2> /dev/null
