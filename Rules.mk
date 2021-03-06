# First, and therefore default, target.
all: targets

# We will need to change this when we make the OCaml-C interface.
OCAMLBUILD := ocamlbuild
OCAMLLIBS := graph

# The next three are to compact some of the dependencies in the top
# level.
grammar_types := grammar-noweb/Grammar_types.ml

fernandez := fernandez-ocaml-noweb/Fernandez_modules.ml

dir := c
include $(dir)/Rules.mk
dir := fernandez-ocaml-noweb
include $(dir)/Rules.mk
dir := grammar-noweb
include $(dir)/Rules.mk
dir := utilities
include $(dir)/Rules.mk
dir := relations
include $(dir)/Rules.mk
dir := zone-valuation-graph
include $(dir)/Rules.mk
dir := clutter
include $(dir)/Rules.mk
dir := thesis
include $(dir)/Rules.mk

# Top level dependencies.

.PHONY: targets
targets: \
calc.native test.native compare_automata.native \
reltool.native thesis/thesis.pdf \
reltool

COMMON_DEPS := \
Rules.mk \
_tags \
myocamlbuild.ml

CALC_NATIVE_DEPS := \
zone-valuation-graph/calc.ml \
zone-valuation-graph/ZVG_modules.ml \
$(grammar_types) \
grammar-noweb/Timed_automaton_lexer.mll \
grammar-noweb/Timed_automaton_parser.mly \
utilities/Parse_timed_automaton.ml \
fernandez-ocaml-noweb/Fernandez_modules.ml \
$(COMMON_DEPS)

COMPARE_AUTOMATA_DEPS := \
zone-valuation-graph/compare_automata.ml \
relations/Relations.ml \
relations/STAB.ml \
relations/TADB.ml \
relations/TAOB.ml \
utilities/Table_using_list.ml \
$(COMMON_DEPS)

NEXT_STEP_DEPS := \
clutter/next_step/next_step.ml \
$(grammar_types) \
grammar-noweb/Timed_automaton_lexer.mll \
grammar-noweb/Timed_automaton_parser.mly \
utilities/Parse_timed_automaton.ml \
$(fernandez) \
c/zone_stubs.mli \
c/zone_stubs.c \
utilities/UDBM_utilities.ml \
$(COMMON_DEPS)

TEST_DEPS := \
$(grammar_types) \
utilities/Clock_constraint_utilities.ml \
utilities/UDBM_utilities.ml \
test/test.ml \
test/Test_base.ml \
zone-valuation-graph/Graph_functions2.ml \
clutter/Graph_functions3.ml \
clutter/Graph_functions3_test.ml \
clutter/Graph_functions2_clutter.ml \
clutter/ZVG_tree.ml \
c/zone_stubs.c \
c/zone_stubs.mli \
c/Zone_stubs_test.ml \
utilities/NRQueue.ml \
utilities/PCQueue_test.ml \
$(COMMON_DEPS)

RELTOOL_DEPS := \
reltool.ml \
$(COMPARE_AUTOMATA_DEPS) \
$(CALC_NATIVE_DEPS) \
$(TEST_DEPS) \
$(COMMON_DEPS)

libzone_stubs.a: \
c/zone_stubs.mli \
c/zone.mllib \
c/zone_stubs.c
	$(OCAMLBUILD) libzone_stubs.a

compare_automata.native: $(COMPARE_AUTOMATA_DEPS)

calc.native: $(CALC_NATIVE_DEPS)

next_step.native: $(NEXT_STEP_DEPS)

test.native: $(TEST_DEPS)

%.native: \
$(COMPARE_AUTOMATA_DEPS) $(CALC_NATIVE_DEPS) \
$(TEST_DEPS) $(RELTOOL_DEPS)
	$(OCAMLBUILD)  \
	zone-valuation-graph/calc.native \
	test/test.native \
	zone-valuation-graph/compare_automata.native \
	reltool.native \

reltool: reltool.native
	objcopy reltool.native reltool
