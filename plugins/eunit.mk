# Copyright (c) 2014, Enrique Fernandez <enrique.fernandez@erlang-solutions.com>
# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is contributed to erlang.mk and subject to the terms of the ISC License.

.PHONY: eunit apps-eunit

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
	case eunit:test($1, [$(EUNIT_OPTS)]) of
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

EUNIT_PATHS = -pa $(TEST_DIR) $(DEPS_DIR)/*/ebin $(APPS_DIR)/*/ebin ebin

ifdef t
ifeq (,$(findstring :,$(t)))
eunit: test-build
	$(gen_verbose) $(call erlang,$(call eunit.erl,['$(t)']),$(EUNIT_PATHS))
else
eunit: test-build
	$(gen_verbose) $(call erlang,$(call eunit.erl,fun $(t)/0),$(EUNIT_PATHS))
endif
else
EUNIT_EBIN_MODS = $(notdir $(basename $(call core_find,ebin/,*.beam)))
EUNIT_TEST_MODS = $(notdir $(basename $(call core_find,$(TEST_DIR)/,*.beam)))
EUNIT_MODS = $(foreach mod,$(EUNIT_EBIN_MODS) $(filter-out \
	$(patsubst %,%_tests,$(EUNIT_EBIN_MODS)),$(EUNIT_TEST_MODS)),'$(mod)')

eunit: test-build $(if $(IS_APP),,apps-eunit)
	$(gen_verbose) $(call erlang,$(call eunit.erl,[$(call comma_list,$(EUNIT_MODS))]),$(EUNIT_PATHS))

ifneq ($(ALL_APPS_DIRS),)
apps-eunit:
	$(verbose) for app in $(ALL_APPS_DIRS); do $(MAKE) -C $$app eunit IS_APP=1; done
endif
endif
