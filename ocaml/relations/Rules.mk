sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

TGT_$(d) := \
$(d)/Relations.ml \
$(d)/Relations.ml.tex \
$(d)/STAB.ml \
$(d)/STAB.ml.tex \
$(d)/TADB.ml \
$(d)/TADB.ml.tex \
$(d)/TAOB.ml \
$(d)/TAOB.ml.tex

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

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
