# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: relx-rel distclean-relx-rel distclean-relx run

# Configuration.

RELX ?= $(CURDIR)/relx
RELX_CONFIG ?= $(CURDIR)/relx.config

RELX_URL ?= https://github.com/erlware/relx/releases/download/v3.5.0/relx
RELX_OPTS ?=
RELX_OUTPUT_DIR ?= _rel

ifeq ($(firstword $(RELX_OPTS)),-o)
	RELX_OUTPUT_DIR = $(word 2,$(RELX_OPTS))
else
	RELX_OPTS += -o $(RELX_OUTPUT_DIR)
endif

# Core targets.

ifeq ($(IS_DEP),)
ifneq ($(wildcard $(RELX_CONFIG)),)
rel:: distclean-relx-rel relx-rel
endif
endif

distclean:: distclean-relx-rel distclean-relx

# Plugin-specific targets.

$(RELX):
	$(gen_verbose) $(call core_http_get,$(RELX),$(RELX_URL))
	$(verbose) chmod +x $(RELX)

relx-rel: $(RELX)
	$(verbose) $(RELX) -c $(RELX_CONFIG) $(RELX_OPTS)

distclean-relx-rel:
	$(gen_verbose) rm -rf $(RELX_OUTPUT_DIR)

distclean-relx:
	$(gen_verbose) rm -rf $(RELX)

# Run target.

ifeq ($(wildcard $(RELX_CONFIG)),)
run:
else

define get_relx_release.erl
	{ok, Config} = file:consult("$(RELX_CONFIG)"),
	{release, {Name, _}, _} = lists:keyfind(release, 1, Config),
	io:format("~s", [Name]),
	halt(0).
endef

RELX_RELEASE = `$(call erlang,$(get_relx_release.erl))`

run: all
	$(verbose) $(RELX_OUTPUT_DIR)/$(RELX_RELEASE)/bin/$(RELX_RELEASE) console

help::
	$(verbose) printf "%s\n" "" \
		"Relx targets:" \
		"  run         Compile the project, build the release and run it"

endif
