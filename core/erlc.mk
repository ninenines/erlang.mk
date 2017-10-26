# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: clean-app

# Configuration.

ERLC_OPTS ?= -Werror +debug_info +warn_export_vars +warn_shadow_vars \
	+warn_obsolete_guard # +bin_opt_info +warn_export_all +warn_missing_spec
COMPILE_FIRST ?=
COMPILE_FIRST_PATHS = $(addprefix src/,$(addsuffix .erl,$(COMPILE_FIRST)))
ERLC_EXCLUDE ?=
ERLC_EXCLUDE_PATHS = $(addprefix src/,$(addsuffix .erl,$(ERLC_EXCLUDE)))

ERLC_ASN1_OPTS ?=

ERLC_MIB_OPTS ?=
COMPILE_MIB_FIRST ?=
COMPILE_MIB_FIRST_PATHS = $(addprefix mibs/,$(addsuffix .mib,$(COMPILE_MIB_FIRST)))

# Verbosity.

app_verbose_0 = @echo " APP   " $(PROJECT);
app_verbose_2 = set -x;
app_verbose = $(app_verbose_$(V))

appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose_2 = set -x;
appsrc_verbose = $(appsrc_verbose_$(V))

makedep_verbose_0 = @echo " DEPEND" $(PROJECT).d;
makedep_verbose_2 = set -x;
makedep_verbose = $(makedep_verbose_$(V))

erlc_verbose_0 = @echo " ERLC  " $(filter-out $(patsubst %,%.erl,$(ERLC_EXCLUDE)),\
	$(filter %.erl %.core,$(?F)));
erlc_verbose_2 = set -x;
erlc_verbose = $(erlc_verbose_$(V))

xyrl_verbose_0 = @echo " XYRL  " $(filter %.xrl %.yrl,$(?F));
xyrl_verbose_2 = set -x;
xyrl_verbose = $(xyrl_verbose_$(V))

asn1_verbose_0 = @echo " ASN1  " $(filter %.asn1,$(?F));
asn1_verbose_2 = set -x;
asn1_verbose = $(asn1_verbose_$(V))

mib_verbose_0 = @echo " MIB   " $(filter %.bin %.mib,$(?F));
mib_verbose_2 = set -x;
mib_verbose = $(mib_verbose_$(V))

ifneq ($(wildcard src/),)

# Targets.

ifeq ($(wildcard ebin/test),)
app:: deps $(PROJECT).d
	$(verbose) $(MAKE) --no-print-directory app-build
else
app:: clean deps $(PROJECT).d
	$(verbose) $(MAKE) --no-print-directory app-build
endif

ifeq ($(wildcard src/$(PROJECT_MOD).erl),)
define app_file
{application, '$(PROJECT)', [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},$(if $(IS_DEP),
	{id$(comma)$(space)"$(1)"}$(comma))
	{modules, [$(call comma_list,$(2))]},
	{registered, []},
	{applications, [$(call comma_list,kernel stdlib $(OTP_DEPS) $(LOCAL_DEPS) $(foreach dep,$(DEPS),$(call dep_name,$(dep))))]},
	{env, $(subst \,\\,$(PROJECT_ENV))}$(if $(findstring {,$(PROJECT_APP_EXTRA_KEYS)),$(comma)$(newline)$(tab)$(subst \,\\,$(PROJECT_APP_EXTRA_KEYS)),)
]}.
endef
else
define app_file
{application, '$(PROJECT)', [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},$(if $(IS_DEP),
	{id$(comma)$(space)"$(1)"}$(comma))
	{modules, [$(call comma_list,$(2))]},
	{registered, [$(call comma_list,$(PROJECT)_sup $(PROJECT_REGISTERED))]},
	{applications, [$(call comma_list,kernel stdlib $(OTP_DEPS) $(LOCAL_DEPS) $(foreach dep,$(DEPS),$(call dep_name,$(dep))))]},
	{mod, {$(PROJECT_MOD), []}},
	{env, $(subst \,\\,$(PROJECT_ENV))}$(if $(findstring {,$(PROJECT_APP_EXTRA_KEYS)),$(comma)$(newline)$(tab)$(subst \,\\,$(PROJECT_APP_EXTRA_KEYS)),)
]}.
endef
endif

app-build: ebin/$(PROJECT).app
	$(verbose) :

# Source files.

ALL_SRC_FILES := $(sort $(call core_find,src/,*))

ERL_FILES := $(filter %.erl,$(ALL_SRC_FILES))
CORE_FILES := $(filter %.core,$(ALL_SRC_FILES))

# ASN.1 files.

ifneq ($(wildcard asn1/),)
ASN1_FILES = $(sort $(call core_find,asn1/,*.asn1))
ERL_FILES += $(addprefix src/,$(patsubst %.asn1,%.erl,$(notdir $(ASN1_FILES))))

define compile_asn1
	$(verbose) mkdir -p include/
	$(asn1_verbose) erlc -v -I include/ -o asn1/ +noobj $(ERLC_ASN1_OPTS) $(1)
	$(verbose) mv asn1/*.erl src/
	$(verbose) mv asn1/*.hrl include/
	$(verbose) mv asn1/*.asn1db include/
endef

$(PROJECT).d:: $(ASN1_FILES)
	$(if $(strip $?),$(call compile_asn1,$?))
endif

# SNMP MIB files.

ifneq ($(wildcard mibs/),)
MIB_FILES = $(sort $(call core_find,mibs/,*.mib))

$(PROJECT).d:: $(COMPILE_MIB_FIRST_PATHS) $(MIB_FILES)
	$(verbose) mkdir -p include/ priv/mibs/
	$(mib_verbose) erlc -v $(ERLC_MIB_OPTS) -o priv/mibs/ -I priv/mibs/ $?
	$(mib_verbose) erlc -o include/ -- $(addprefix priv/mibs/,$(patsubst %.mib,%.bin,$(notdir $?)))
endif

# Leex and Yecc files.

XRL_FILES := $(filter %.xrl,$(ALL_SRC_FILES))
XRL_ERL_FILES = $(addprefix src/,$(patsubst %.xrl,%.erl,$(notdir $(XRL_FILES))))
ERL_FILES += $(XRL_ERL_FILES)

YRL_FILES := $(filter %.yrl,$(ALL_SRC_FILES))
YRL_ERL_FILES = $(addprefix src/,$(patsubst %.yrl,%.erl,$(notdir $(YRL_FILES))))
ERL_FILES += $(YRL_ERL_FILES)

$(PROJECT).d:: $(XRL_FILES) $(YRL_FILES)
	$(if $(strip $?),$(xyrl_verbose) erlc -v -o src/ $(YRL_ERLC_OPTS) $?)

# Erlang and Core Erlang files.

define makedep.erl
	E = ets:new(makedep, [bag]),
	G = digraph:new([acyclic]),
	ErlFiles = lists:usort(string:tokens("$(ERL_FILES)", " ")),
	Modules = [{list_to_atom(filename:basename(F, ".erl")), F} || F <- ErlFiles],
	Add = fun (Mod, Dep) ->
		case lists:keyfind(Dep, 1, Modules) of
			false -> ok;
			{_, DepFile} ->
				{_, ModFile} = lists:keyfind(Mod, 1, Modules),
				ets:insert(E, {ModFile, DepFile}),
				digraph:add_vertex(G, Mod),
				digraph:add_vertex(G, Dep),
				digraph:add_edge(G, Mod, Dep)
		end
	end,
	AddHd = fun (F, Mod, DepFile) ->
		case file:open(DepFile, [read]) of
			{error, enoent} -> ok;
			{ok, Fd} ->
				F(F, Fd, Mod),
				{_, ModFile} = lists:keyfind(Mod, 1, Modules),
				ets:insert(E, {ModFile, DepFile})
		end
	end,
	Attr = fun
		(F, Mod, behavior, Dep) -> Add(Mod, Dep);
		(F, Mod, behaviour, Dep) -> Add(Mod, Dep);
		(F, Mod, compile, {parse_transform, Dep}) -> Add(Mod, Dep);
		(F, Mod, compile, Opts) when is_list(Opts) ->
			case proplists:get_value(parse_transform, Opts) of
				undefined -> ok;
				Dep -> Add(Mod, Dep)
			end;
		(F, Mod, include, Hrl) ->
			case filelib:is_file("include/" ++ Hrl) of
				true -> AddHd(F, Mod, "include/" ++ Hrl);
				false ->
					case filelib:is_file("src/" ++ Hrl) of
						true -> AddHd(F, Mod, "src/" ++ Hrl);
						false -> false
					end
			end;
		(F, Mod, include_lib, "$1/include/" ++ Hrl) -> AddHd(F, Mod, "include/" ++ Hrl);
		(F, Mod, include_lib, Hrl) -> AddHd(F, Mod, "include/" ++ Hrl);
		(F, Mod, import, {Imp, _}) ->
			IsFile =
				case lists:keyfind(Imp, 1, Modules) of
					false -> false;
					{_, FilePath} -> filelib:is_file(FilePath)
				end,
			case IsFile of
				false -> ok;
				true -> Add(Mod, Imp)
			end;
		(_, _, _, _) -> ok
	end,
	MakeDepend = fun(F, Fd, Mod) ->
		case io:parse_erl_form(Fd, undefined) of
			{ok, {attribute, _, Key, Value}, _} ->
				Attr(F, Mod, Key, Value),
				F(F, Fd, Mod);
			{eof, _} ->
				file:close(Fd);
			_ ->
				F(F, Fd, Mod)
		end
	end,
	[begin
		Mod = list_to_atom(filename:basename(F, ".erl")),
		{ok, Fd} = file:open(F, [read]),
		MakeDepend(MakeDepend, Fd, Mod)
	end || F <- ErlFiles],
	Depend = sofs:to_external(sofs:relation_to_family(sofs:relation(ets:tab2list(E)))),
	CompileFirst = [X || X <- lists:reverse(digraph_utils:topsort(G)), [] =/= digraph:in_neighbours(G, X)],
	TargetPath = fun(Target) ->
		case lists:keyfind(Target, 1, Modules) of
			false -> "";
			{_, DepFile} ->
				DirSubname = tl(string:tokens(filename:dirname(DepFile), "/")),
				string:join(DirSubname ++ [atom_to_list(Target)], "/")
		end
	end,
	ok = file:write_file("$(1)", [
		[[F, "::", [[" ", D] || D <- Deps], "; @touch \$$@\n"] || {F, Deps} <- Depend],
		"\nCOMPILE_FIRST +=", [[" ", TargetPath(CF)] || CF <- CompileFirst], "\n"
	]),
	halt()
endef

ifeq ($(if $(NO_MAKEDEP),$(wildcard $(PROJECT).d),),)
$(PROJECT).d:: $(ERL_FILES) $(call core_find,include/,*.hrl) $(MAKEFILE_LIST)
	$(makedep_verbose) $(call erlang,$(call makedep.erl,$@))
endif

ifneq ($(words $(ERL_FILES) $(CORE_FILES) $(ASN1_FILES) $(MIB_FILES) $(XRL_FILES) $(YRL_FILES)),0)
# Rebuild everything when the Makefile changes.
$(ERLANG_MK_TMP)/last-makefile-change: $(MAKEFILE_LIST)
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(verbose) if test -f $@; then \
		touch $(ERL_FILES) $(CORE_FILES) $(ASN1_FILES) $(MIB_FILES) $(XRL_FILES) $(YRL_FILES); \
		touch -c $(PROJECT).d; \
	fi
	$(verbose) touch $@

$(ERL_FILES) $(CORE_FILES) $(ASN1_FILES) $(MIB_FILES) $(XRL_FILES) $(YRL_FILES):: $(ERLANG_MK_TMP)/last-makefile-change
ebin/$(PROJECT).app:: $(ERLANG_MK_TMP)/last-makefile-change
endif

include $(wildcard $(PROJECT).d)

ebin/$(PROJECT).app:: ebin/

ebin/:
	$(verbose) mkdir -p ebin/

define compile_erl
	$(erlc_verbose) erlc -v $(if $(IS_DEP),$(filter-out -Werror,$(ERLC_OPTS)),$(ERLC_OPTS)) -o ebin/ \
		-pa ebin/ -I include/ $(filter-out $(ERLC_EXCLUDE_PATHS),$(COMPILE_FIRST_PATHS) $(1))
endef

ebin/$(PROJECT).app:: $(ERL_FILES) $(CORE_FILES) $(wildcard src/$(PROJECT).app.src)
	$(eval FILES_TO_COMPILE := $(filter-out src/$(PROJECT).app.src,$?))
	$(if $(strip $(FILES_TO_COMPILE)),$(call compile_erl,$(FILES_TO_COMPILE)))
	$(eval GITDESCRIBE := $(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null || true))
	$(eval MODULES := $(patsubst %,'%',$(sort $(notdir $(basename \
		$(filter-out $(ERLC_EXCLUDE_PATHS),$(ERL_FILES) $(CORE_FILES) $(BEAM_FILES)))))))
ifeq ($(wildcard src/$(PROJECT).app.src),)
	$(app_verbose) printf '$(subst %,%%,$(subst $(newline),\n,$(subst ','\'',$(call app_file,$(GITDESCRIBE),$(MODULES)))))' \
		> ebin/$(PROJECT).app
else
	$(verbose) if [ -z "$$(grep -e '^[^%]*{\s*modules\s*,' src/$(PROJECT).app.src)" ]; then \
		echo "Empty modules entry not found in $(PROJECT).app.src. Please consult the erlang.mk README for instructions." >&2; \
		exit 1; \
	fi
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed "s/{[[:space:]]*modules[[:space:]]*,[[:space:]]*\[\]}/{modules, \[$(call comma_list,$(MODULES))\]}/" \
		| sed "s/{id,[[:space:]]*\"git\"}/{id, \"$(subst /,\/,$(GITDESCRIBE))\"}/" \
		> ebin/$(PROJECT).app
endif

clean:: clean-app

clean-app:
	$(gen_verbose) rm -rf $(PROJECT).d ebin/ priv/mibs/ $(XRL_ERL_FILES) $(YRL_ERL_FILES) \
		$(addprefix include/,$(patsubst %.mib,%.hrl,$(notdir $(MIB_FILES)))) \
		$(addprefix include/,$(patsubst %.asn1,%.hrl,$(notdir $(ASN1_FILES)))) \
		$(addprefix include/,$(patsubst %.asn1,%.asn1db,$(notdir $(ASN1_FILES)))) \
		$(addprefix src/,$(patsubst %.asn1,%.erl,$(notdir $(ASN1_FILES))))

endif
