PDFS = $(patsubst %.org, %.pdf, $(wildcard *.org))

.DEFAULT_GOAL := default
.PHONY: tangle test

tangle: *.org
	$(foreach org-file, $^, emacs --batch --find-file $(org-file) --funcall org-babel-tangle --kill)

%.tex: %.org
	emacs --batch --find-file $< --funcall org-beamer-export-to-latex --kill

%.pdf: %.tex
	pdflatex $<

test: tangle
	stack test

.PHONY: all
default: test $(PDFS)

.PHONY: clean
clean:
	rm *.tex *.pdf
