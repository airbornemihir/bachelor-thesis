all: targets

FIGURES := breaking2.pdf_tex \
breaking2-zones-abstracted.pdf_tex \
breaking2-zones01.pdf_tex \
breaking2-zones02.pdf_tex \
breaking2-zones03.pdf_tex \
breaking2.pdf_tex \
breaking2-zones-quotient.pdf_tex \
pair03first.pdf_tex \
pair03second.pdf_tex \
pair03first-quotient.pdf_tex \
pair03second-quotient.pdf_tex
TARGETS := presentation.pdf

.PHONY: targets
targets: $(TARGETS)

$(TARGETS): Makefile

%.svg : %.dot
	dot -Tsvg $< -o $@

%.png : %.dot
	dot -Tpng $< -o $@

%.pdf_tex: %.svg
	inkscape -D -z --file=$< --export-pdf=$(subst pdf_tex,pdf,$@) --export-latex

presentation.pdf: presentation.tex presentation.bib $(FIGURES)
	pdflatex \\nonstopmode\\input presentation.tex
	bibtex --min-crossref=100 presentation
	pdflatex \\nonstopmode\\input presentation.tex
	pdflatex \\nonstopmode\\input presentation.tex
