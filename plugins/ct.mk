# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: ct apps-ct distclean-ct

# Configuration.

CT_OPTS ?=
ifneq ($(wildcard $(TEST_DIR)),)
	CT_SUITES ?= $(sort $(subst _SUITE.erl,,$(notdir $(call core_find,$(TEST_DIR)/,*_SUITE.erl))))
else
	CT_SUITES ?=
endif

# Core targets.

tests:: ct

distclean:: distclean-ct

help::
	$(verbose) printf "%s\n" "" \
		"Common_test targets:" \
		"  ct          Run all the common_test suites for this project" \
		"" \
		"All your common_test suites have their associated targets." \
		"A suite named http_SUITE can be ran using the ct-http target."

# Plugin-specific targets.

CT_RUN = ct_run \
	-no_auto_compile \
	-noinput \
	-pa $(CURDIR)/ebin $(DEPS_DIR)/*/ebin $(TEST_DIR) \
	-dir $(TEST_DIR) \
	-logdir $(CURDIR)/logs

ifeq ($(CT_SUITES),)
ct: $(if $(IS_APP),,apps-ct)
else
ct: test-build $(if $(IS_APP),,apps-ct)
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)
endif

ifneq ($(ALL_APPS_DIRS),)
apps-ct:
	$(verbose) for app in $(ALL_APPS_DIRS); do $(MAKE) -C $$app ct IS_APP=1; done
endif

ifndef t
CT_EXTRA =
else
ifeq (,$(findstring :,$t))
CT_EXTRA = -group $t
else
t_words = $(subst :, ,$t)
CT_EXTRA = -group $(firstword $(t_words)) -case $(lastword $(t_words))
endif
endif

define ct_suite_target
ct-$(1): test-build
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(1)) $(CT_EXTRA) $(CT_OPTS)
endef

$(foreach test,$(CT_SUITES),$(eval $(call ct_suite_target,$(test))))

distclean-ct:
	$(gen_verbose) rm -rf $(CURDIR)/logs/
