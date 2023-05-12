# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2014, M Robert Martin <rob@version2beta.com>
# This file is contributed to erlang.mk and subject to the terms of the ISC License.

.PHONY: shell

# Configuration.

SHELL_ERL ?= erl
SHELL_PATHS ?= $(CURDIR)/ebin $(TEST_DIR)
SHELL_OPTS ?=

ALL_SHELL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(SHELL_DEPS))

# Core targets

help::
	$(verbose) printf "%s\n" "" \
		"Shell targets:" \
		"  shell       Run an erlang shell with SHELL_OPTS or reasonable default"

# Plugin-specific targets.

$(foreach dep,$(SHELL_DEPS),$(eval $(call dep_target,$(dep))))

ifneq ($(SKIP_DEPS),)
build-shell-deps:
else
build-shell-deps: $(ALL_SHELL_DEPS_DIRS)
	$(verbose) set -e; for dep in $(ALL_SHELL_DEPS_DIRS) ; do \
		if [ -z "$(strip $(FULL))" ] && [ ! -L $$dep ] && [ -f $$dep/ebin/dep_built ]; then \
			:; \
		else \
			$(MAKE) -C $$dep IS_DEP=1 ELIXIR_USE_SYSTEM=$(ELIXIR_USE_SYSTEM); \
			if [ ! -L $$dep ] && [ -d $$dep/ebin ]; then touch $$dep/ebin/dep_built; fi; \
		fi \
	done
endif

shell:: build-shell-deps
	$(gen_verbose) $(SHELL_ERL) -pa $(SHELL_PATHS) $(SHELL_OPTS)
