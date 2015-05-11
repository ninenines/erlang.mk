# Copyright (c) 2015, Ilya Khaprov <ilya.khaprov@publitechs.com>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Configuration.

ELIXIRC = elixirc
ELIXIRC_OPTS ?= --warnings-as-errors
ELIXIRC_OUT = -o ebin/

# Core targets.

define compile_elixir
	ERL_COMPILER_OPTS="$(ERLC_OPTS)" $(ELIXIRC) $(ELIXIRC_OPTS) $(ELIXIRC_OUT) $(1)
endef

ifneq ($(wildcard src/),)
ebin/$(PROJECT).app:: $(shell find src -type f -name \*.ex 2>/dev/null)
	$(if $(strip $?),$(call compile_elixir,$?))
endif
