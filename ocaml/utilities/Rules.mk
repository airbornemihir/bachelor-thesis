sp := $(sp).x
dirstack_$(sp) := $(d)
d := $(dir)

TGT_$(d) := $(d)/Clock_constraint_utilities.ml $(d)/NRQueue.ml \
$(d)/Table_using_list.ml $(d)/UDBM_utilities.ml

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/Clock_constraint_utilities.ml: \
$(d)/Clock_constraint_utilities.ml.nw
	notangle -RClock_constraint_utilities.ml \
$(d)/Clock_constraint_utilities.ml.nw > $(d)/Clock_constraint_utilities.ml

$(d)/NRQueue.ml: \
$(d)/NRQueue.ml.nw
	notangle -RNRQueue.ml $(d)/NRQueue.ml.nw > $(d)/NRQueue.ml

$(d)/Table_using_list.ml: \
$(d)/Table_using_list.ml.nw
	notangle -RTable_using_list.ml $(d)/Table_using_list.ml.nw > $(d)/Table_using_list.ml

$(d)/UDBM_utilities.ml: \
$(d)/UDBM_utilities.ml.nw
	notangle -RUDBM_utilities.ml $(d)/UDBM_utilities.ml.nw > $(d)/UDBM_utilities.ml

# The argument passed with the -R switch does not require
# qualification with $(d) because it refers solely to the file name
# that appears before each code chunk.
d := $(dirstack_$(sp))
sp := $(basename $(sp))
