ALL_FIGURE_NAMES=$(shell cat paper_fatima.figlist)
ALL_FIGURES=$(ALL_FIGURE_NAMES:%=%.pdf)

allimages: $(ALL_FIGURES)
	@echo All images exist now. Use make -B to re-generate them.

FORCEREMAKE:

include $(ALL_FIGURE_NAMES:%=%.dep)

%.dep:
	mkdir -p "$(dir $@)"
	touch "$@" # will be filled later.

paper_fatima-figure0.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "paper_fatima-figure0" "\def\tikzexternalrealjob{paper_fatima}\input{paper_fatima}"

paper_fatima-figure0.pdf: paper_fatima-figure0.md5
paper_fatima-figure1.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "paper_fatima-figure1" "\def\tikzexternalrealjob{paper_fatima}\input{paper_fatima}"

paper_fatima-figure1.pdf: paper_fatima-figure1.md5
