# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: relx-rel relx-relup distclean-relx-rel run

# Configuration.

RELX ?= $(ERLANG_MK_TMP)/relx
RELX_CONFIG ?= $(CURDIR)/relx.config

RELX_URL ?= https://erlang.mk/res/relx-v3.26.0
RELX_OPTS ?=
RELX_OUTPUT_DIR ?= _rel
RELX_REL_EXT ?=
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
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(gen_verbose) $(call core_http_get,$(RELX),$(RELX_URL))
	$(verbose) chmod +x $(RELX)

relx-rel: $(RELX) rel-deps app
	$(verbose) $(RELX) $(if $(filter 1,$V),-V 3) -c $(RELX_CONFIG) $(RELX_OPTS) release
	$(verbose) $(MAKE) relx-post-rel
ifeq ($(RELX_TAR),1)
	$(verbose) $(RELX) $(if $(filter 1,$V),-V 3) -c $(RELX_CONFIG) $(RELX_OPTS) tar
endif

relx-relup: $(RELX) rel-deps app
	$(verbose) $(RELX) $(if $(filter 1,$V),-V 3) -c $(RELX_CONFIG) $(RELX_OPTS) release
	$(MAKE) relx-post-rel
	$(verbose) $(RELX) $(if $(filter 1,$V),-V 3) -c $(RELX_CONFIG) $(RELX_OPTS) relup $(if $(filter 1,$(RELX_TAR)),tar)

distclean-relx-rel:
	$(gen_verbose) rm -rf $(RELX_OUTPUT_DIR)

# Default hooks.
relx-post-rel::
	$(verbose) :

# Run target.

ifeq ($(wildcard $(RELX_CONFIG)),)
run::
else

define get_relx_release.erl
	{ok, Config} = file:consult("$(call core_native_path,$(RELX_CONFIG))"),
	{release, {Name, Vsn0}, _} = lists:keyfind(release, 1, Config),
	Vsn = case Vsn0 of
		{cmd, Cmd} -> os:cmd(Cmd);
		semver -> "";
		{semver, _} -> "";
		VsnStr -> Vsn0
	end,
	Extended = case lists:keyfind(extended_start_script, 1, Config) of
		{_, true} -> "1";
		_ -> ""
	end,
	io:format("~s ~s ~s", [Name, Vsn, Extended]),
	halt(0).
endef

RELX_REL := $(shell $(call erlang,$(get_relx_release.erl)))
RELX_REL_NAME := $(word 1,$(RELX_REL))
RELX_REL_VSN := $(word 2,$(RELX_REL))
RELX_REL_CMD := $(if $(word 3,$(RELX_REL)),console)

ifeq ($(PLATFORM),msys2)
RELX_REL_EXT := .cmd
endif

run:: all
	$(verbose) $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/bin/$(RELX_REL_NAME)$(RELX_REL_EXT) $(RELX_REL_CMD)

ifdef RELOAD
rel::
	$(verbose) $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/bin/$(RELX_REL_NAME)$(RELX_REL_EXT) ping
	$(verbose) $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/bin/$(RELX_REL_NAME)$(RELX_REL_EXT) \
		eval "io:format(\"~p~n\", [c:lm()])"
endif

help::
	$(verbose) printf "%s\n" "" \
		"Relx targets:" \
		"  run         Compile the project, build the release and run it"

endif
