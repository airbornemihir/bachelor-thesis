all: targets

FIGURES := lts01.pdf_tex lts01quotient.pdf_tex breaking2.pdf_tex \
breaking2-zones01.pdf_tex breaking2-zones02.pdf_tex \
breaking2-zones03.pdf_tex breaking2-zones-abstracted.pdf_tex \
pair01first.pdf_tex pair01second.pdf_tex \
pair02first.pdf_tex pair02second.pdf_tex \
pair03first.pdf_tex pair03second.pdf_tex \
regiongraph01.pdf_tex regiongraph02.pdf_tex
TARGETS := thesis.pdf

.PHONY: targets
targets: $(TARGETS)

$(TARGETS): Makefile

%.svg : %.dot
	dot -Tsvg $< -o $@

%.png : %.dot
	dot -Tpng $< -o $@

%.pdf_tex: %.svg
	inkscape -D -z --file=$< --export-pdf=$(subst pdf_tex,pdf,$@) --export-latex

thesis.pdf: thesis.tex thesis.bib $(FIGURES)
	-pdflatex \\nonstopmode\\input thesis.tex
	bibtex --min-crossref=100 thesis
	-pdflatex \\nonstopmode\\input thesis.tex
	-pdflatex \\nonstopmode\\input thesis.tex
