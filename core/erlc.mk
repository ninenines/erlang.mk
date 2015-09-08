# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: clean-app

# Configuration.

ERLC_OPTS ?= -Werror +debug_info +warn_export_vars +warn_shadow_vars \
	+warn_obsolete_guard # +bin_opt_info +warn_export_all +warn_missing_spec
COMPILE_FIRST ?=
COMPILE_FIRST_PATHS = $(addprefix src/,$(addsuffix .erl,$(COMPILE_FIRST)))
ERLC_EXCLUDE ?=
ERLC_EXCLUDE_PATHS = $(addprefix src/,$(addsuffix .erl,$(ERLC_EXCLUDE)))

ERLC_MIB_OPTS ?=
COMPILE_MIB_FIRST ?=
COMPILE_MIB_FIRST_PATHS = $(addprefix mibs/,$(addsuffix .mib,$(COMPILE_MIB_FIRST)))

# Verbosity.

app_verbose_0 = @echo " APP   " $(PROJECT);
app_verbose = $(app_verbose_$(V))

appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose = $(appsrc_verbose_$(V))

makedep_verbose_0 = @echo " DEPEND" $(PROJECT).d;
makedep_verbose = $(makedep_verbose_$(V))

erlc_verbose_0 = @echo " ERLC  " $(filter-out $(patsubst %,%.erl,$(ERLC_EXCLUDE)),\
	$(filter %.erl %.core,$(?F)));
erlc_verbose = $(erlc_verbose_$(V))

xyrl_verbose_0 = @echo " XYRL  " $(filter %.xrl %.yrl,$(?F));
xyrl_verbose = $(xyrl_verbose_$(V))

asn1_verbose_0 = @echo " ASN1  " $(filter %.asn1,$(?F));
asn1_verbose = $(asn1_verbose_$(V))

mib_verbose_0 = @echo " MIB   " $(filter %.bin %.mib,$(?F));
mib_verbose = $(mib_verbose_$(V))

ifneq ($(wildcard src/),)

# Targets.

ifeq ($(wildcard ebin/test),)
app:: $(PROJECT).d
	$(verbose) $(MAKE) --no-print-directory app-build
else
app:: clean $(PROJECT).d
	$(verbose) $(MAKE) --no-print-directory app-build
endif

ifeq ($(wildcard src/$(PROJECT)_app.erl),)
define app_file
{application, $(PROJECT), [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},
	{id, "$(1)"},
	{modules, [$(call comma_list,$(2))]},
	{registered, []},
	{applications, [$(call comma_list,kernel stdlib $(OTP_DEPS) $(DEPS))]}
]}.
endef
else
define app_file
{application, $(PROJECT), [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},
	{id, "$(1)"},
	{modules, [$(call comma_list,$(2))]},
	{registered, [$(call comma_list,$(PROJECT)_sup $(PROJECT_REGISTERED))]},
	{applications, [$(call comma_list,kernel stdlib $(OTP_DEPS) $(DEPS))]},
	{mod, {$(PROJECT)_app, []}}
]}.
endef
endif

app-build: ebin/$(PROJECT).app ; @echo -n

# Source files.

ERL_FILES = $(sort $(call core_find,src/,*.erl))
CORE_FILES = $(sort $(call core_find,src/,*.core))

# ASN.1 files.

ifneq ($(wildcard asn1/),)
ASN1_FILES = $(sort $(call core_find,asn1/,*.asn1))
ERL_FILES += $(addprefix src/,$(patsubst %.asn1,%.erl,$(notdir $(ASN1_FILES))))

define compile_asn1
	$(verbose) mkdir -p include/
	$(asn1_verbose) erlc -v -I include/ -o asn1/ +noobj $(1)
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

XRL_FILES = $(sort $(call core_find,src/,*.xrl))
XRL_ERL_FILES = $(addprefix src/,$(patsubst %.xrl,%.erl,$(notdir $(XRL_FILES))))
ERL_FILES += $(XRL_ERL_FILES)

YRL_FILES = $(sort $(call core_find,src/,*.yrl))
YRL_ERL_FILES = $(addprefix src/,$(patsubst %.yrl,%.erl,$(notdir $(YRL_FILES))))
ERL_FILES += $(YRL_ERL_FILES)

$(PROJECT).d:: $(XRL_FILES) $(YRL_FILES)
	$(if $(strip $?),$(xyrl_verbose) erlc -v -o src/ $?)

# Erlang and Core Erlang files.

define makedep.erl
	ErlFiles = lists:usort(string:tokens("$(ERL_FILES)", " ")),
	Modules = [{filename:basename(F, ".erl"), F} || F <- ErlFiles],
	Add = fun (Dep, Acc) ->
		case lists:keyfind(atom_to_list(Dep), 1, Modules) of
			{_, DepFile} -> [DepFile|Acc];
			false -> Acc
		end
	end,
	AddHd = fun (Dep, Acc) ->
		case {Dep, lists:keymember(Dep, 2, Modules)} of
			{"src/" ++ _, false} -> [Dep|Acc];
			{"include/" ++ _, false} -> [Dep|Acc];
			_ -> Acc
		end
	end,
	CompileFirst = fun (Deps) ->
		First0 = [case filename:extension(D) of
			".erl" -> filename:basename(D, ".erl");
			_ -> []
		end || D <- Deps],
		case lists:usort(First0) of
			[] -> [];
			[[]] -> [];
			First -> ["COMPILE_FIRST +=", [[" ", F] || F <- First], "\n"]
		end
	end,
	Depend = [begin
		case epp:parse_file(F, ["include/"], []) of
			{ok, Forms} ->
				Deps = lists:usort(lists:foldl(fun
					({attribute, _, behavior, Dep}, Acc) -> Add(Dep, Acc);
					({attribute, _, behaviour, Dep}, Acc) -> Add(Dep, Acc);
					({attribute, _, compile, {parse_transform, Dep}}, Acc) -> Add(Dep, Acc);
					({attribute, _, file, {Dep, _}}, Acc) -> AddHd(Dep, Acc);
					(_, Acc) -> Acc
				end, [], Forms)),
				case Deps of
					[] -> "";
					_ -> [F, "::", [[" ", D] || D <- Deps], "; @touch \$$@\n", CompileFirst(Deps)]
				end;
			{error, enoent} ->
				[]
		end
	end || F <- ErlFiles],
	ok = file:write_file("$(1)", Depend),
	halt()
endef

ifeq ($(if $(NO_MAKEDEP),$(wildcard $(PROJECT).d),),)
$(PROJECT).d:: $(ERL_FILES) $(call core_find,include/,*.hrl)
	$(makedep_verbose) $(call erlang,$(call makedep.erl,$@))
endif

-include $(PROJECT).d

ebin/$(PROJECT).app:: ebin/

ebin/:
	$(verbose) mkdir -p ebin/

define compile_erl
	$(erlc_verbose) erlc -v $(if $(IS_DEP),$(filter-out -Werror,$(ERLC_OPTS)),$(ERLC_OPTS)) -o ebin/ \
		-pa ebin/ -I include/ $(filter-out $(ERLC_EXCLUDE_PATHS),$(COMPILE_FIRST_PATHS) $(1))
endef

ebin/$(PROJECT).app:: $(ERL_FILES) $(CORE_FILES)
	$(if $(strip $?),$(call compile_erl,$?))
	$(eval GITDESCRIBE := $(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null || true))
	$(eval MODULES := $(patsubst %,'%',$(sort $(notdir $(basename $(ERL_FILES) $(CORE_FILES))))))
ifeq ($(wildcard src/$(PROJECT).app.src),)
	$(app_verbose) echo $(subst $(newline),,$(subst ",\",$(call app_file,$(GITDESCRIBE),$(MODULES)))) \
		> ebin/$(PROJECT).app
else
	$(verbose) if [ -z "$$(grep -E '^[^%]*{\s*modules\s*,' src/$(PROJECT).app.src)" ]; then \
		echo "Empty modules entry not found in $(PROJECT).app.src. Please consult the erlang.mk README for instructions." >&2; \
		exit 1; \
	fi
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed "s/{[[:space:]]*modules[[:space:]]*,[[:space:]]*\[\]}/{modules, \[$(call comma_list,$(MODULES))\]}/" \
		| sed "s/{id,[[:space:]]*\"git\"}/{id, \"$(GITDESCRIBE)\"}/" \
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
