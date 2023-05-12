# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: test-deps test-dir test-build clean-test-dir

# Configuration.

TEST_DIR ?= $(CURDIR)/test

ALL_TEST_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(TEST_DEPS))

TEST_ERLC_OPTS ?= +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
TEST_ERLC_OPTS += -DTEST=1

# Targets.

$(foreach dep,$(TEST_DEPS),$(eval $(call dep_target,$(dep))))

ifneq ($(SKIP_DEPS),)
test-deps:
else
test-deps: $(ALL_TEST_DEPS_DIRS)
	$(verbose) set -e; for dep in $(ALL_TEST_DEPS_DIRS) ; do \
		if [ -z "$(strip $(FULL))" ] && [ ! -L $$dep ] && [ -f $$dep/ebin/dep_built ]; then \
			:; \
		else \
			$(MAKE) -C $$dep IS_DEP=1 ELIXIR_USE_SYSTEM=$(ELIXIR_USE_SYSTEM); \
			if [ ! -L $$dep ] && [ -d $$dep/ebin ]; then touch $$dep/ebin/dep_built; fi; \
		fi \
	done
endif

ifneq ($(wildcard $(TEST_DIR)),)
test-dir: $(ERLANG_MK_TMP)/$(PROJECT).last-testdir-build
	@:

test_erlc_verbose_0 = @echo " ERLC  " $(filter-out $(patsubst %,%.erl,$(ERLC_EXCLUDE)),\
	$(filter %.erl %.core,$(notdir $(FILES_TO_COMPILE))));
test_erlc_verbose_2 = set -x;
test_erlc_verbose = $(test_erlc_verbose_$(V))

define compile_test_erl
	$(test_erlc_verbose) erlc -v $(TEST_ERLC_OPTS) -o $(TEST_DIR) \
		-pa ebin/ -I include/ $(1)
endef

ERL_TEST_FILES = $(call core_find,$(TEST_DIR)/,*.erl)

$(ERLANG_MK_TMP)/$(PROJECT).last-testdir-build: $(ERL_TEST_FILES) $(MAKEFILE_LIST)
# When we have to recompile files in src/ the .d file always gets rebuilt.
# Therefore we want to ignore it when rebuilding test files.
	$(eval FILES_TO_COMPILE := $(if $(filter $(filter-out $(PROJECT).d,$(MAKEFILE_LIST)),$?),$(filter $(ERL_TEST_FILES),$^),$(filter $(ERL_TEST_FILES),$?)))
	$(if $(strip $(FILES_TO_COMPILE)),$(call compile_test_erl,$(FILES_TO_COMPILE)) && touch $@)
endif

test-build:: IS_TEST=1
test-build:: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build:: $(if $(wildcard src),$(if $(wildcard ebin/test),,beam-cache-restore-test)) $(if $(IS_APP),,deps test-deps)
# We already compiled everything when IS_APP=1.
ifndef IS_APP
ifneq ($(wildcard src),)
	$(verbose) $(MAKE) --no-print-directory $(PROJECT).d ERLC_OPTS="$(call escape_dquotes,$(TEST_ERLC_OPTS))"
	$(verbose) $(MAKE) --no-print-directory app-build ERLC_OPTS="$(call escape_dquotes,$(TEST_ERLC_OPTS))"
	$(gen_verbose) touch ebin/test
endif
ifneq ($(wildcard $(TEST_DIR)),)
	$(verbose) $(MAKE) --no-print-directory test-dir ERLC_OPTS="$(call escape_dquotes,$(TEST_ERLC_OPTS))"
endif
endif

# Roughly the same as test-build, but when IS_APP=1.
# We only care about compiling the current application.
ifdef IS_APP
test-build-app:: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-app:: deps test-deps
ifneq ($(wildcard src),)
	$(verbose) $(MAKE) --no-print-directory $(PROJECT).d ERLC_OPTS="$(call escape_dquotes,$(TEST_ERLC_OPTS))"
	$(verbose) $(MAKE) --no-print-directory app-build ERLC_OPTS="$(call escape_dquotes,$(TEST_ERLC_OPTS))"
	$(gen_verbose) touch ebin/test
endif
ifneq ($(wildcard $(TEST_DIR)),)
	$(verbose) $(MAKE) --no-print-directory test-dir ERLC_OPTS="$(call escape_dquotes,$(TEST_ERLC_OPTS))"
endif
endif

clean:: clean-test-dir

clean-test-dir:
ifneq ($(wildcard $(TEST_DIR)/*.beam),)
	$(gen_verbose) rm -f $(TEST_DIR)/*.beam $(ERLANG_MK_TMP)/$(PROJECT).last-testdir-build
endif
