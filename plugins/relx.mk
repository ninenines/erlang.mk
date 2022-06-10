# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifeq ($(filter relx,$(BUILD_DEPS) $(DEPS) $(REL_DEPS)),relx)
.PHONY: relx-rel relx-relup distclean-relx-rel run

# Configuration.

RELX_CONFIG ?= $(CURDIR)/relx.config

RELX_OUTPUT_DIR ?= _rel
RELX_REL_EXT ?=
RELX_TAR ?= 1

ifdef SFX
	RELX_TAR = 1
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

define relx_release.erl
	{ok, Config} = file:consult("$(call core_native_path,$(RELX_CONFIG))"),
	{release, {Name, Vsn0}, _} = lists:keyfind(release, 1, Config),
	Vsn = case Vsn0 of
		{cmd, Cmd} -> os:cmd(Cmd);
		semver -> "";
		{semver, _} -> "";
		{git, short} -> string:trim(os:cmd("git rev-parse --short HEAD"), both, "\n");
		{git, long} -> string:trim(os:cmd("git rev-parse HEAD"), both, "\n");
		VsnStr -> Vsn0
	end,
	{ok, _} = relx:build_release(#{name => Name, vsn => Vsn}, Config),
	halt(0).
endef

define relx_tar.erl
	{ok, Config} = file:consult("$(call core_native_path,$(RELX_CONFIG))"),
	{release, {Name, Vsn0}, _} = lists:keyfind(release, 1, Config),
	Vsn = case Vsn0 of
		{cmd, Cmd} -> os:cmd(Cmd);
		semver -> "";
		{semver, _} -> "";
		{git, short} -> string:trim(os:cmd("git rev-parse --short HEAD"), both, "\n");
		{git, long} -> string:trim(os:cmd("git rev-parse HEAD"), both, "\n");
		VsnStr -> Vsn0
	end,
	{ok, _} = relx:build_tar(#{name => Name, vsn => Vsn}, Config),
	halt(0).
endef

define relx_relup.erl
	{ok, Config} = file:consult("$(call core_native_path,$(RELX_CONFIG))"),
	{release, {Name, Vsn0}, _} = lists:keyfind(release, 1, Config),
	Vsn = case Vsn0 of
		{cmd, Cmd} -> os:cmd(Cmd);
		semver -> "";
		{semver, _} -> "";
		{git, short} -> string:trim(os:cmd("git rev-parse --short HEAD"), both, "\n");
		{git, long} -> string:trim(os:cmd("git rev-parse HEAD"), both, "\n");
		VsnStr -> Vsn0
	end,
	{ok, _} = relx:build_relup(Name, Vsn, undefined, Config ++ [{output_dir, "$(RELX_OUTPUT_DIR)"}]),
	halt(0).
endef

relx-rel: rel-deps app
	$(call erlang,$(call relx_release.erl),-pa ebin/)
	$(verbose) $(MAKE) relx-post-rel
ifeq ($(RELX_TAR),1)
	$(call erlang,$(call relx_tar.erl),-pa ebin/)
endif

relx-relup: rel-deps app
	$(call erlang,$(call relx_release.erl),-pa ebin/)
	$(MAKE) relx-post-rel
	$(call erlang,$(call relx_relup.erl),-pa ebin/)
ifeq ($(RELX_TAR),1)
	$(call erlang,$(call relx_tar.erl),-pa ebin/)
endif

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
		{git, short} -> string:trim(os:cmd("git rev-parse --short HEAD"), both, "\n");
		{git, long} -> string:trim(os:cmd("git rev-parse HEAD"), both, "\n");
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
endif
