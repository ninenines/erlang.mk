# Copyright (c) 2014, Enrique Fernandez <enrique.fernandez@erlang-solutions.com>
# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is contributed to erlang.mk and subject to the terms of the ISC License.

ifeq ($(findstring eunit,$(ERLANG_MK_DISABLE_PLUGINS)),)

.PHONY: eunit

# Configuration

EUNIT_OPTS ?=

# Core targets.

tests:: eunit

help::
	$(verbose) printf "%s\n" "" \
		"EUnit targets:" \
		"  eunit       Run all the EUnit tests for this project"

# Plugin-specific targets.

define eunit.erl
	case "$(COVER)" of
		"" -> ok;
		_ ->
			case cover:compile_beam_directory("ebin") of
				{error, _} -> halt(1);
				_ -> ok
			end
	end,
	case eunit:test([$(call comma_list,$(1))], [$(EUNIT_OPTS)]) of
		ok -> ok;
		error -> halt(2)
	end,
	case "$(COVER)" of
		"" -> ok;
		_ ->
			cover:export("eunit.coverdata")
	end,
	halt()
endef

EUNIT_EBIN_MODS = $(notdir $(basename $(call core_find,ebin/,*.beam)))
EUNIT_TEST_MODS = $(notdir $(basename $(call core_find,$(TEST_DIR)/,*.beam)))
EUNIT_MODS = $(foreach mod,$(EUNIT_EBIN_MODS) $(filter-out \
	$(patsubst %,%_tests,$(EUNIT_EBIN_MODS)),$(EUNIT_TEST_MODS)),{module,'$(mod)'})

eunit: test-build
	$(gen_verbose) $(ERL) -pa $(TEST_DIR) $(DEPS_DIR)/*/ebin ebin \
		-eval "$(subst $(newline),,$(subst ",\",$(call eunit.erl,$(EUNIT_MODS))))"

endif # ERLANG_MK_DISABLE_PLUGINS
