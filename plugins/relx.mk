# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: relx-rel relx-relup distclean-relx-rel run

# Configuration.

RELX ?= $(ERLANG_MK_TMP)/relx
RELX_CONFIG ?= $(CURDIR)/relx.config

RELX_URL ?= https://github.com/erlware/relx/releases/download/v3.19.0/relx
RELX_OPTS ?=
RELX_OUTPUT_DIR ?= _rel
RELX_TAR ?= 1

ifdef SFX
	RELX_TAR = 1
endif

ifeq ($(firstword $(RELX_OPTS)),-o)
	RELX_OUTPUT_DIR = $(word 2,$(RELX_OPTS))
else
	RELX_OPTS += -o $(RELX_OUTPUT_DIR)
endif

# Core targets.

ifeq ($(IS_DEP),)
ifneq ($(wildcard $(RELX_CONFIG)),)
rel:: relx-rel

relup:: relx-relup
endif
endif

distclean:: distclean-relx-rel

# Plugin-specific targets.

$(RELX):
	$(gen_verbose) $(call core_http_get,$(RELX),$(RELX_URL))
	$(verbose) chmod +x $(RELX)

relx-rel: $(RELX) rel-deps app
ifeq ($(RELX_TAR),1)
	$(verbose) $(RELX) -c $(RELX_CONFIG) $(RELX_OPTS) release tar
else
	$(verbose) $(RELX) -c $(RELX_CONFIG) $(RELX_OPTS) release
endif

relx-relup: $(RELX) rel-deps app
ifeq ($(RELX_TAR),1)
	$(verbose) $(RELX) -c $(RELX_CONFIG) $(RELX_OPTS) release relup tar
else
	$(verbose) $(RELX) -c $(RELX_CONFIG) $(RELX_OPTS) release relup
endif

distclean-relx-rel:
	$(gen_verbose) rm -rf $(RELX_OUTPUT_DIR)

# Run target.

ifeq ($(wildcard $(RELX_CONFIG)),)
run:
else

define get_relx_release.erl
	{ok, Config} = file:consult("$(RELX_CONFIG)"),
	{release, {Name, Vsn}, _} = lists:keyfind(release, 1, Config),
	io:format("~s ~s", [Name, Vsn]),
	halt(0).
endef

RELX_REL := $(shell $(call erlang,$(get_relx_release.erl)))
RELX_REL_NAME := $(word 1,$(RELX_REL))
RELX_REL_VSN := $(word 2,$(RELX_REL))

run: all
	$(verbose) $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/bin/$(RELX_REL_NAME) console

help::
	$(verbose) printf "%s\n" "" \
		"Relx targets:" \
		"  run         Compile the project, build the release and run it"

endif
