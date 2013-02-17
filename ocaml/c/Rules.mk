sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

TGT_$(d) := $(d)/libzone.a $(d)/zone_stubs.o

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/libzone.a: $(d)/zone_stubs.o
	ar rc $(d)/libzone.a $(d)/zone_stubs.o
	ranlib $(d)/libzone.a

$(d)/zone_stubs.o: $(d)/zone_stubs.c
	$(CPP) $(CFLAGS) -o $@ -c $<	

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
