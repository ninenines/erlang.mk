# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: test-deps test-dir test-build clean-test-dir

# Configuration.

TEST_DIR ?= test

ALL_TEST_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(TEST_DEPS))

TEST_ERLC_OPTS ?= +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
TEST_ERLC_OPTS += -DTEST=1

# Targets.

$(foreach dep,$(TEST_DEPS),$(eval $(call dep_target,$(dep))))

test-deps: $(ALL_TEST_DEPS_DIRS)
	@for dep in $(ALL_TEST_DEPS_DIRS) ; do $(MAKE) -C $$dep; done

ifneq ($(strip $(TEST_DIR)),)
test-dir:
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o $(TEST_DIR) \
		$(wildcard $(TEST_DIR)/*.erl $(TEST_DIR)/*/*.erl) -pa ebin/
endif

ifeq ($(wildcard ebin/test),)
test-build:: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build:: clean deps test-deps
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test
else
test-build:: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build:: deps test-deps
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
endif

clean:: clean-test-dir

clean-test-dir:
ifneq ($(wildcard $(TEST_DIR)/*.beam),)
	$(gen_verbose) rm -f $(TEST_DIR)/*.beam
endif
