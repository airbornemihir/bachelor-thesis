sp := $(sp).x
dirstack_$(sp) := $(d)
d := $(dir)

TGT_$(d) := $(d)/Graph_functions2.ml $(d)/ZVG_modules.ml \
$(d)/ZVG_tree.ml $(d)/calc.ml $(d)/compare_automata.ml

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/Graph_functions2.ml: $(d)/Graph_functions2.ml.nw
	notangle -RGraph_functions2.ml \
$(d)/Graph_functions2.ml.nw > $(d)/Graph_functions2.ml

$(d)/ZVG_modules.ml: $(d)/ZVG_modules.ml.nw
	notangle -RZVG_modules.ml \
$(d)/ZVG_modules.ml.nw > $(d)/ZVG_modules.ml

$(d)/ZVG_tree.ml: $(d)/ZVG_tree.ml.nw
	notangle -RZVG_tree.ml $(d)/ZVG_tree.ml.nw > $(d)/ZVG_tree.ml

$(d)/calc.ml: $(d)/calc.ml.nw
	notangle -Rcalc.ml $(d)/calc.ml.nw > $(d)/calc.ml

$(d)/compare_automata.ml: $(d)/compare_automata.ml.nw
	notangle -Rcompare_automata.ml $(d)/compare_automata.ml.nw \
> $(d)/compare_automata.ml

# The argument passed with the -R switch does not require
# qualification with $(d) because it refers solely to the file name
# that appears before each code chunk.
d := $(dirstack_$(sp))
sp := $(basename $(sp))
