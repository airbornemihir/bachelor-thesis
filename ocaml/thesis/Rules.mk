sp := $(sp).x
dirstack_$(sp) := $(d)
d := $(dir)

FIGURES := $(d)/lts01.pdf_tex \
$(d)/lts01quotient.pdf_tex \
$(d)/breaking2.pdf_tex \
$(d)/breaking2-zones01.pdf_tex \
$(d)/breaking2-zones02.pdf_tex \
$(d)/breaking2-zones03.pdf_tex \
$(d)/breaking2-zones-abstracted.pdf_tex \
$(d)/pair01first.pdf_tex \
$(d)/pair01second.pdf_tex \
$(d)/pair02first.pdf_tex \
$(d)/pair02second.pdf_tex \
$(d)/pair03first.pdf_tex \
$(d)/pair03second.pdf_tex \
$(d)/regiongraph01.pdf_tex \
$(d)/regiongraph02.pdf_tex

SOURCE_TEX := \
$(d)/grammar_types.ml.tex \
$(d)/timed_automaton_parser.mly.tex \
$(d)/timed_automaton_lexer.mll.tex \
$(d)/Clock_constraint_utilities.ml.tex \
$(d)/NRQueue.ml.tex \
$(d)/Table_using_list.ml.tex \
$(d)/UDBM_utilities.ml.tex \
$(d)/parse_timed_automaton.ml.tex \
$(d)/UDBM_utilities.ml.tex \
$(d)/Graph_functions2.ml.tex \
$(d)/ZVG_modules.ml.tex \
$(d)/calc.ml.tex \
$(d)/compare_automata.ml.tex \
$(d)/zone_stubs.c.tex \
$(d)/zone_stubs.mli.tex \
$(d)/Relations.ml.tex \
$(d)/STAB.ml.tex \
$(d)/TADB.ml.tex \
$(d)/TAOB.ml.tex 

TGT_$(d) := $(d)/thesis.pdf $(FIGURES)

.PHONY: targets
targets: $(TGT_$(d))

$(TGT_$(d)): d := $(d)

$(TGT_$(d)): $(d)/Rules.mk

$(d)/%.svg : $(d)/%.dot
	dot -Tsvg $< -o $@

$(d)/%.png : $(d)/%.dot
	dot -Tpng $< -o $@

$(d)/%.pdf_tex: $(d)/%.svg
	inkscape -D -z --file=$< --export-pdf=$(subst pdf_tex,pdf,$@) \
	--export-latex

$(d)/grammar_types.ml.tex: grammar-noweb/grammar_types.ml.tex
	cp $< $@

$(d)/timed_automaton_parser.mly.tex: grammar-noweb/timed_automaton_parser.mly.tex
	cp $< $@

$(d)/timed_automaton_lexer.mll.tex: grammar-noweb/timed_automaton_lexer.mll.tex
	cp $< $@

$(d)/Clock_constraint_utilities.ml.tex : utilities/Clock_constraint_utilities.ml.tex
	cp $< $@

$(d)/NRQueue.ml.tex: utilities/NRQueue.ml.tex
	cp $< $@

$(d)/Table_using_list.ml.tex: utilities/Table_using_list.ml.tex
	cp $< $@

$(d)/UDBM_utilities.ml.tex: utilities/UDBM_utilities.ml.tex
	cp $< $@

$(d)/parse_timed_automaton.ml.tex: utilities/parse_timed_automaton.ml.tex
	cp $< $@

$(d)/Graph_functions2.ml.tex: zone-valuation-graph/Graph_functions2.ml.tex
	cp $< $@

$(d)/ZVG_modules.ml.tex: zone-valuation-graph/ZVG_modules.ml.tex
	cp $< $@

$(d)/calc.ml.tex: zone-valuation-graph/calc.ml.tex
	cp $< $@

$(d)/compare_automata.ml.tex: zone-valuation-graph/compare_automata.ml.tex
	cp $< $@

$(d)/zone_stubs.c.tex: c/zone_stubs.c.tex
	cp $< $@

$(d)/zone_stubs.mli.tex: c/zone_stubs.mli.tex
	cp $< $@

$(d)/Relations.ml.tex: relations/Relations.ml.tex
	cp $< $@

$(d)/STAB.ml.tex: relations/STAB.ml.tex
	cp $< $@

$(d)/TADB.ml.tex: relations/TADB.ml.tex
	cp $< $@

$(d)/TAOB.ml.tex: relations/TAOB.ml.tex
	cp $< $@

$(d)/thesis.pdf: $(d)/thesis.tex $(d)/title.tex $(d)/IITD-LOGO.jpg $(d)/thesis.bib $(FIGURES) \
$(SOURCE_TEX)
	-env TEXINPUTS=./$(d): pdflatex -output-directory $(d) \\nonstopmode\\input $(d)/thesis.tex
	env TEXMFOUTPUT=$(d) BIBINPUTS=$(d) bibtex --min-crossref=100 $(d)/thesis
	-env TEXINPUTS=./$(d): pdflatex -output-directory $(d) \\nonstopmode\\input $(d)/thesis.tex
	-env TEXINPUTS=./$(d): pdflatex -output-directory $(d) \\nonstopmode\\input $(d)/thesis.tex

# The argument passed with the -R switch does not require
# qualification with $(d) because it refers solely to the file name
# that appears before each code chunk.
d := $(dirstack_$(sp))
sp := $(basename $(sp))
