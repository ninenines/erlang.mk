# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: ct distclean-ct

# Configuration.

CT_OPTS ?=
ifneq ($(wildcard $(TEST_DIR)),)
	CT_SUITES ?= $(sort $(subst _SUITE.erl,,$(shell find $(TEST_DIR) -type f -name \*_SUITE.erl -exec basename {} \;)))
else
	CT_SUITES ?=
endif

# Core targets.

tests:: ct

distclean:: distclean-ct

help::
	@printf "%s\n" "" \
		"Common_test targets:" \
		"  ct          Run all the common_test suites for this project" \
		"" \
		"All your common_test suites have their associated targets." \
		"A suite named http_SUITE can be ran using the ct-http target."

# Plugin-specific targets.

CT_RUN = ct_run \
	-no_auto_compile \
	-noinput \
	-pa ebin $(DEPS_DIR)/*/ebin \
	-dir $(TEST_DIR) \
	-logdir logs

ifeq ($(CT_SUITES),)
ct:
else
ct: test-build
	@mkdir -p logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)
endif

define ct_suite_target
ct-$(1): test-build
	@mkdir -p logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(1)) $(CT_OPTS)
endef

$(foreach test,$(CT_SUITES),$(eval $(call ct_suite_target,$(test))))

distclean-ct:
	$(gen_verbose) rm -rf logs/
