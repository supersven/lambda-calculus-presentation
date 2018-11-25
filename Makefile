PDFS = $(patsubst %.org, %.pdf, $(wildcard *.org))

.DEFAULT_GOAL := default
.PHONY: tangle test dot

tangle: *.org
	$(foreach org-file, $^, emacs --batch -l init.el --find-file $(org-file) --funcall org-babel-tangle --kill)

%.tex: %.org
	emacs --batch -l init.el --find-file $< --funcall org-beamer-export-to-latex --kill

%.pdf: %.tex
	xelatex -shell-escape -8bit $<

%.ps: %.dot
	dot -O -Tps $<

dot: $(patsubst %.dot, %.ps, $(wildcard dot/*.dot))
	echo $<

test: tangle
	stack test

.PHONY: all
default: test dot $(PDFS)


.PHONY: clean
clean:
	rm -f *.tex *.tex~ *.pdf *.aux *.log *.nav *.out *.snm *.toc *.vrb
