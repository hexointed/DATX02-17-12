### Makefile for compiling .tex files ###

#All targets
DOCS = $(patsubst %.tex,%.pdf,$(wildcard *.tex))
IMGS = $(patsubst %.svg,%.pdf,$(wildcard resources/*.svg))

#Fail silently
MAKEFLAGS = --silent

docs: $(IMGS) $(DOCS) 

docs-dbg: $(IMGS) $(DOCS)

no-imgs: $(DOCS) 

resources/%.pdf: resources/%.svg
	inkscape -D -z --file=$^ --export-pdf=$@

%.pdf: %.tex
	- pdflatex $^ > ./log.txt
	- bibtex $*   > ./log.txt
	- pdflatex $^ > ./log.txt
	- pdflatex $^ > ./log.txt

#Remove all temporary files created by pdfLaTeX
clean: 
	- rm *.pdf 2> /dev/null
	- rm *.lof *.lot *.out *.toc *.aux *.bbl *.blg *.log 2> /dev/null
	- rm resources/*.pdf 2> /dev/null
