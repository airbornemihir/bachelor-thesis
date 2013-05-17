sp := $(sp).x
dirstack_$(sp) := $(d)
d := $(dir)

TGT_$(d) := $(d)/next_step.ml

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

# The argument passed with the -R switch does not require
# qualification with $(d) because it refers solely to the file name
# that appears before each code chunk.
$(d)/next_step.ml: $(d)/next_step.nw
	notangle -Rnext_step.ml $(d)/next_step.nw \
	>$(d)/next_step.ml

d := $(dirstack_$(sp))
sp := $(basename $(sp))
