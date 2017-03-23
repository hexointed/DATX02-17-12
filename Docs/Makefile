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
	convert $^ $@ >> ./log.txt
#	inkscape -D -z --file=$(shell pwd)/$^ --export-pdf=$(shell pwd)/$@ >> ./log.txt

%.pdf: %.tex
	- pdflatex $^ >> ./log.txt
	- bibtex $*   >> ./log.txt
	- pdflatex $^ >> ./log.txt
	- pdflatex $^ >> ./log.txt

#Remove all temporary files created by pdfLaTeX
clean: 
	- rm *.pdf 2> /dev/null
	- rm *.lof *.lot *.out *.toc *.aux *.bbl *.blg *.log 2> /dev/null
	- rm figure/*.pdf 2> /dev/null
	- touch log.txt
