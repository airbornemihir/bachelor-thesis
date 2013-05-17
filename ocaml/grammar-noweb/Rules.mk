sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

TGT_$(d) := $(d)/timed_automaton_lexer.mll \
$(d)/timed_automaton_lexer.mll.tex \
$(d)/timed_automaton_parser.mly \
$(d)/timed_automaton_parser.mly.tex \
$(d)/grammar_types.ml \
$(d)/grammar_types.ml.tex \
$(d)/calc.native

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/timed_automaton_lexer.mll:$(d)/timed_automaton_lexer.mll.nw
	notangle -Rtimed_automaton_lexer.mll $(d)/timed_automaton_lexer.mll.nw >$(d)/timed_automaton_lexer.mll

$(d)/timed_automaton_parser.mly:$(d)/timed_automaton_parser.mly.nw
	notangle -Rtimed_automaton_parser.mly $(d)/timed_automaton_parser.mly.nw >$(d)/timed_automaton_parser.mly

$(d)/grammar_types.ml:$(d)/grammar_types.ml.nw
	notangle -Rgrammar_types.ml $(d)/grammar_types.ml.nw \
>$(d)/grammar_types.ml

$(d)/%.tex: $(d)/%.nw
	noweave -n -filter 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"' -latex $< > $@

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
