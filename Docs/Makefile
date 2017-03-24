### Makefile for compiling .tex files ###

#All targets
DOCS = $(patsubst %.tex,%.pdf,$(wildcard *.tex))
IMGS = $(patsubst %.svg,%.pdf,$(wildcard figure/*.svg))

#Fail silently
MAKEFLAGS = --silent

docs: $(IMGS) $(DOCS) 

docs-dbg: $(IMGS) $(DOCS)

no-imgs: $(DOCS) 

#NOTE(bjorn) "convert" is from lib ImageMagick. Couldn't get inkscape to work properly on OSX.
figure/%.pdf: figure/%.svg
	convert $^ $@ >> ./log.log
#	inkscape -D -z --file=$(shell pwd)/$^ --export-pdf=$(shell pwd)/$@ >> ./log.log

%.pdf: %.tex
	- pdflatex $^ >> ./log.log
	- bibtex $*   >> ./log.log
	- pdflatex $^ >> ./log.log
	- pdflatex $^ >> ./log.log

#Remove all temporary files created by pdfLaTeX
clean: 
	- rm *.pdf 2> /dev/null
	- rm *.lof *.lot *.out *.toc *.aux *.bbl *.blg *.log 2> /dev/null
	- rm figure/*.pdf 2> /dev/null
