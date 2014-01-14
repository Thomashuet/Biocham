# BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
# Copyright 2003-2006, INRIA, Projet Contraintes
# Makefile

examplesdir = $(pkgdatadir)/EXAMPLES

EX_DIRS = cell_cycle cell_cycle_Tyson_6Var MAPK small small_cell_cycle kinetics locations
FULL_EX_DIRS = $(EX_DIRS) $(wildcard LEARNING/*) generic_test
EXTRA_DIST = .htaccess $(foreach dir,$(EX_DIRS),$(wildcard $(dir)/*.bc))
nobase_examples_DATA = $(EXTRA_DIST)

TESTS = $(wildcard *_test.sh) $(foreach dir,$(FULL_EX_DIRS),$(wildcard $(dir)/*_test.sh))

clean:
	$(RM) *~ */*~
