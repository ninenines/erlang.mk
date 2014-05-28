# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: build-ct-deps build-ct-suites tests-ct clean-ct distclean-ct

# Configuration.

CT_OPTS ?=
CT_SUITES ?=

TEST_ERLC_OPTS ?= +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
TEST_ERLC_OPTS += -DTEST=1 -DEXTRA=1 +'{parse_transform, eunit_autoexport}'

# Core targets.

tests:: tests-ct

clean:: clean-ct

distclean:: distclean-ct

help::
	@printf "%s\n" "" \
		"All your common_test suites have their associated targets." \
		"A suite named http_SUITE can be ran using the ct-http target."

# Plugin-specific targets.

ALL_TEST_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(TEST_DEPS))

CT_RUN = ct_run \
	-no_auto_compile \
	-noshell \
	-pa $(realpath ebin) $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs \
	$(CT_OPTS)

$(foreach dep,$(TEST_DEPS),$(eval $(call dep_target,$(dep))))

build-ct-deps: $(ALL_TEST_DEPS_DIRS)
	@for dep in $(ALL_TEST_DEPS_DIRS) ; do $(MAKE) -C $$dep; done

build-ct-suites: build-ct-deps
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -o test/ \
		$(wildcard test/*.erl test/*/*.erl) -pa ebin/

tests-ct: ERLC_OPTS = $(TEST_ERLC_OPTS)
tests-ct: clean deps app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) ; \
	fi
	$(gen_verbose) rm -f test/*.beam

define ct_suite_target
ct-$(1): ERLC_OPTS = $(TEST_ERLC_OPTS)
ct-$(1): clean deps app build-tests
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(1)) ; \
	fi
	$(gen_verbose) rm -f test/*.beam
endef

$(foreach test,$(CT_SUITES),$(eval $(call ct_suite_target,$(test))))

clean-ct:
	$(gen_verbose) rm -rf test/*.beam

distclean-ct:
	$(gen_verbose) rm -rf logs/
