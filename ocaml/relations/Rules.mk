sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

TGT_$(d) := $(d)/Relations.ml $(d)/STAB.ml $(d)/TADB.ml $(d)/TAOB.ml

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/Relations.ml: $(d)/Relations.ml.nw
	notangle -RRelations.ml $(d)/Relations.ml.nw > $(d)/Relations.ml

$(d)/STAB.ml: $(d)/STAB.ml.nw
	notangle -RSTAB.ml $(d)/STAB.ml.nw > $(d)/STAB.ml

$(d)/TADB.ml: $(d)/TADB.ml.nw
	notangle -RTADB.ml $(d)/TADB.ml.nw > $(d)/TADB.ml

$(d)/TAOB.ml: $(d)/TAOB.ml.nw
	notangle -RTAOB.ml $(d)/TAOB.ml.nw > $(d)/TAOB.ml

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
