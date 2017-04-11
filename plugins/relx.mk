# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: relx-rel relx-relup distclean-relx-rel run

# Configuration.

RELX ?= $(ERLANG_MK_TMP)/relx
RELX_CONFIG ?= $(CURDIR)/relx.config
RELX_CONFIG_SCRIPT ?= $(RELX_CONFIG).script

RELX_URL ?= https://github.com/erlware/relx/releases/download/v3.19.0/relx
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
	$(verbose) $(RELX) -c $(RELX_CONFIG) $(RELX_OPTS) release tar

relx-relup: $(RELX) rel-deps app
	$(verbose) $(RELX) -c $(RELX_CONFIG) $(RELX_OPTS) release relup tar

distclean-relx-rel:
	$(gen_verbose) rm -rf $(RELX_OUTPUT_DIR)

# Run target.

ifeq ($(wildcard $(RELX_CONFIG)),)
run:
else

define get_relx_release.erl
	{ok, Config0} = file:consult("$(RELX_CONFIG)"),
	{ok, Config} = case file:read_link("$(RELX_CONFIG_SCRIPT)") of
		{ok, _} ->
			Bindings = erl_eval:add_binding(
					'CONFIG', Config0,
					erl_eval:add_binding(
						'SCRIPT', "$(RELX_CONFIG_SCRIPT)",
						erl_eval:new_binding())),
			file:script("$(RELX_CONFIG_SCRIPT)", Bindings);
		{error, _} ->
			{ok, Config0}
	end,
	{release, {Name, Vsn0}, _} = lists:keyfind(release, 1, Config),
	MakeSemVer = fun (P) ->
		Tag0 = string:strip(
			os:cmd("git log --oneline --decorate 2>/dev/null |"
				" grep -F \\"tag: " ++ P ++ "\\" |"
				" head -n 1 |"
				" sed -n \\"s/.*tag: " ++ P ++ "\\\\([^,)]*\\\\).*/\\\\1/p\\""),
			both, $$\n),
		Tag = case Tag0 of
			"" -> "0.0.0";
			_ -> Tag0
		end,
		RawRef = string:strip(os:cmd("git log -n 1 --pretty=format:'%h' 2>/dev/null"), both, $$\n),
		RefTag = [".ref", re:replace(RawRef, "\\s", "", [global])],
		RawCount = case Tag of
			"" -> os:cmd("git rev-list HEAD 2>/dev/null | wc -l");
			_ -> os:cmd(io_lib:format("git rev-list \\"" ++ P ++ "~s\\"..HEAD 2>/dev/null | wc -l", [Tag0]))
		end,
		Count = erlang:iolist_to_binary(
			  re:replace(
			    string:strip(RawCount, both, $$\n),
			    "\\s", "", [global]
			  )
			),
		CompTag = case Count of
			<<"0">> -> Tag;
			<<"fatal", _/binary>> -> [Tag, RefTag];
			_ -> [Tag, "+build.", Count, RefTag]
		end,
		erlang:binary_to_list(erlang:iolist_to_binary(CompTag))
	end,
	Vsn = case Vsn0 of
		{cmd, Cmd} -> os:cmd(Cmd);
		{semver, Prefix} -> MakeSemVer(Prefix);
		semver -> MakeSemVer("v");
		VsnString -> VsnString
	end,
	io:format("~s ~s", [Name, Vsn]),
	halt(0).
endef

RELX_REL := $(shell $(call erlang,$(get_relx_release.erl)))
RELX_REL_NAME := $(word 1,$(RELX_REL))
RELX_REL_VSN := $(word 2,$(RELX_REL))

run: all
	$(verbose) $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/bin/$(RELX_REL_NAME) console

relvsn:
	$(verbose) printf "%s\n" "$(RELX_REL_VSN)"

help::
	$(verbose) printf "%s\n" "" \
		"Relx targets:" \
		"  run         Compile the project, build the release and run it"
		"  relvsn      Print the version of the current release"

endif
