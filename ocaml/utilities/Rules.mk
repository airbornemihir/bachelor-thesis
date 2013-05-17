sp := $(sp).x
dirstack_$(sp) := $(d)
d := $(dir)

TGT_$(d) := $(d)/Clock_constraint_utilities.ml \
$(d)/Clock_constraint_utilities.ml.tex \
$(d)/NRQueue.ml \
$(d)/NRQueue.ml.tex \
$(d)/Table_using_list.ml \
$(d)/Table_using_list.ml.tex \
$(d)/UDBM_utilities.ml \
$(d)/UDBM_utilities.ml.tex \
$(d)/parse_timed_automaton.ml \
$(d)/parse_timed_automaton.ml.tex

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
	noweave -n -filter 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"' -latex $< > $@

# The argument passed with the -R switch does not require
# qualification with $(d) because it refers solely to the file name
# that appears before each code chunk.
d := $(dirstack_$(sp))
sp := $(basename $(sp))
