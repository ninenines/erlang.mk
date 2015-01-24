# Copyright (c) 2014, Enrique Fernandez <enrique.fernandez@erlang-solutions.com>
# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is contributed to erlang.mk and subject to the terms of the ISC License.

.PHONY: eunit

# Configuration

# All modules in TEST_DIR
ifeq ($(strip $(TEST_DIR)),)
TEST_DIR_MODS = 
else
TEST_DIR_MODS = $(notdir $(basename $(shell find $(TEST_DIR) -type f -name *.beam)))
endif

# All modules in 'ebin'
EUNIT_EBIN_MODS = $(notdir $(basename $(shell find ebin -type f -name *.beam)))
# Only those modules in TEST_DIR with no matching module in 'ebin'.
# This is done to avoid some tests being executed twice.
EUNIT_MODS = $(filter-out $(patsubst %,%_tests,$(EUNIT_EBIN_MODS)),$(TEST_DIR_MODS))
TAGGED_EUNIT_TESTS = $(foreach mod,$(EUNIT_EBIN_MODS) $(EUNIT_MODS),{module,$(mod)})

EUNIT_OPTS ?= verbose

# Utility functions

define str-join
	$(shell echo '$(strip $(1))' | sed -e "s/ /,/g")
endef

# Core targets.

tests:: eunit

help::
	@printf "%s\n" "" \
		"EUnit targets:" \
		"  eunit       Run all the EUnit tests for this project"

# Plugin-specific targets.

EUNIT_RUN_BEFORE ?=
EUNIT_RUN_AFTER ?=
EUNIT_RUN = $(ERL) \
	-pa $(TEST_DIR) $(DEPS_DIR)/*/ebin \
	-pz ebin \
	$(EUNIT_RUN_BEFORE) \
	-eval 'case eunit:test([$(call str-join,$(TAGGED_EUNIT_TESTS))],\
		[$(EUNIT_OPTS)]) of ok -> ok; error -> halt(1) end.' \
	$(EUNIT_RUN_AFTER) \
	-eval 'halt(0).'

eunit: test-build
	$(gen_verbose) $(EUNIT_RUN)
