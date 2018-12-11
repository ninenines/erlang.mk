# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: ct apps-ct distclean-ct

# Configuration.

CT_OPTS ?=

ifneq ($(wildcard $(TEST_DIR)),)
ifndef CT_SUITES
CT_SUITES := $(sort $(subst _SUITE.erl,,$(notdir $(call core_find,$(TEST_DIR)/,*_SUITE.erl))))
endif
endif
CT_SUITES ?=
CT_LOGS_DIR ?= $(CURDIR)/logs

# Core targets.

tests:: ct

ifndef KEEP_LOGS
distclean:: distclean-ct
endif

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
	-pa $(CURDIR)/ebin $(TEST_DIR) \
	-dir $(TEST_DIR) \
	-logdir $(CT_LOGS_DIR)

ifeq ($(CT_SUITES),)
ct: $(if $(IS_APP)$(ROOT_DIR),,apps-ct)
else
# We do not run tests if we are in an apps/* with no test directory.
ifneq ($(IS_APP)$(wildcard $(TEST_DIR)),1)
ct: test-build $(if $(IS_APP)$(ROOT_DIR),,apps-ct)
	$(verbose) mkdir -p $(CT_LOGS_DIR)
	$(gen_verbose) $(CT_RUN) -sname ct_$(PROJECT) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)
endif
endif

ifneq ($(ALL_APPS_DIRS),)
define ct_app_target
apps-ct-$1: test-build
	$$(MAKE) -C $1 ct IS_APP=1
endef

$(foreach app,$(ALL_APPS_DIRS),$(eval $(call ct_app_target,$(app))))

apps-ct: $(addprefix apps-ct-,$(ALL_APPS_DIRS))
endif

ifdef t
ifeq (,$(findstring :,$t))
CT_EXTRA = -group $t
else
t_words = $(subst :, ,$t)
CT_EXTRA = -group $(firstword $(t_words)) -case $(lastword $(t_words))
endif
else
ifdef c
CT_EXTRA = -case $c
else
CT_EXTRA =
endif
endif

define ct_suite_target
ct-$(1): test-build
	$(verbose) mkdir -p $(CT_LOGS_DIR)
	$(gen_verbose_esc) $(CT_RUN) -sname ct_$(PROJECT) -suite $(addsuffix _SUITE,$(1)) $(CT_EXTRA) $(CT_OPTS)
endef

$(foreach test,$(CT_SUITES),$(eval $(call ct_suite_target,$(test))))

distclean-ct:
	$(gen_verbose) rm -rf $(CT_LOGS_DIR)
