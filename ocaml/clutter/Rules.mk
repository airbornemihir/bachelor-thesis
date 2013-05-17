sp := $(sp).x
dirstack_$(sp) := $(d)
d := $(dir)

dir := $(d)/next_step
include $(dir)/Rules.mk

TGT_$(d) := $(d)/ZVG_tree.ml \
$(d)/ZVG_tree.ml.tex

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/%.ml: $(d)/%.ml.nw
	notangle -R$(subst $(d)/,,$@) $< > $@

$(d)/%.ml.tex: $(d)/%.ml.nw
	noweave -filter 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"' -index -latex $< > $@

# The argument passed with the -R switch does not require
# qualification with $(d) because it refers solely to the file name
# that appears before each code chunk.
d := $(dirstack_$(sp))
sp := $(basename $(sp))
