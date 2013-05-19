sp := $(sp).x
dirstack_$(sp) := $(d)
d := $(dir)

TGT_$(d) := $(d)/Fernandez_modules.ml $(d)/Alt_Fernandez_modules.ml

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/Fernandez_modules.ml: $(d)/Fernandez_modules.nw
	notangle -RFernandez_modules.ml $(d)/Fernandez_modules.nw \
	>$(d)/Fernandez_modules.ml

$(d)/Alt_Fernandez_modules.ml: $(d)/Alt_Fernandez_modules.nw
	notangle -RAlt_Fernandez_modules.ml $(d)/Alt_Fernandez_modules.nw \
	>$(d)/Alt_Fernandez_modules.ml

d := $(dirstack_$(sp))
sp := $(basename $(sp))
