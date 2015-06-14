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

erlc_verbose_0 = @echo " ERLC  " $(filter-out $(patsubst %,%.erl,$(ERLC_EXCLUDE)),\
	$(filter %.erl %.core,$(?F)));
erlc_verbose = $(erlc_verbose_$(V))

xyrl_verbose_0 = @echo " XYRL  " $(filter %.xrl %.yrl,$(?F));
xyrl_verbose = $(xyrl_verbose_$(V))

asn1_verbose_0 = @echo " ASN1  " $(filter %.asn1,$(?F));
asn1_verbose = $(asn1_verbose_$(V))

mib_verbose_0 = @echo " MIB   " $(filter %.bin %.mib,$(?F));
mib_verbose = $(mib_verbose_$(V))

# Targets.

ifeq ($(wildcard ebin/test),)
app:: app-build
else
app:: clean app-build
endif

ifeq ($(wildcard src/$(PROJECT)_app.erl),)
define app_file
{application, $(PROJECT), [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},
	{id, "$(1)"},
	{modules, [$(MODULES)]},
	{registered, []},
	{applications, $(call erlang_list,kernel stdlib $(OTP_DEPS) $(DEPS))}
]}.
endef
else
define app_file
{application, $(PROJECT), [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},
	{id, "$(1)"},
	{modules, [$(MODULES)]},
	{registered, $(call erlang_list,$(PROJECT)_sup $(PROJECT_REGISTERED))},
	{applications, $(call erlang_list,kernel stdlib $(OTP_DEPS) $(DEPS))},
	{mod, {$(PROJECT)_app, []}}
]}.
endef
endif

app-build: erlc-include ebin/$(PROJECT).app
	$(eval MODULES := $(shell find ebin -type f -name \*.beam \
		| sed "s/ebin\//'/;s/\.beam/',/" | sed '$$s/.$$//'))
	$(eval GITDESCRIBE := $(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null || true))
ifeq ($(wildcard src/$(PROJECT).app.src),)
	$(app_verbose) echo $(subst $(newline),,$(subst ",\",$(call app_file,$(GITDESCRIBE),$(MODULES)))) \
		> ebin/$(PROJECT).app
else
	@if [ -z "$$(grep -E '^[^%]*{\s*modules\s*,' src/$(PROJECT).app.src)" ]; then \
		echo "Empty modules entry not found in $(PROJECT).app.src. Please consult the erlang.mk README for instructions." >&2; \
		exit 1; \
	fi
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed "s/{modules,[[:space:]]*\[\]}/{modules, \[$(MODULES)\]}/" \
		| sed "s/{id,[[:space:]]*\"git\"}/{id, \"$(GITDESCRIBE)\"}/" \
		> ebin/$(PROJECT).app
endif

erlc-include:
	-@if [ -d ebin/ ]; then \
		find include/ src/ -type f -name \*.hrl -newer ebin -exec touch $(shell find src/ -type f -name "*.erl") \; 2>/dev/null || printf ''; \
	fi

define compile_erl
	$(erlc_verbose) erlc -v $(ERLC_OPTS) -o ebin/ \
		-pa ebin/ -I include/ $(filter-out $(ERLC_EXCLUDE_PATHS),\
		$(COMPILE_FIRST_PATHS) $(1))
endef

define compile_xyrl
	$(xyrl_verbose) erlc -v -o ebin/ $(1)
	$(xyrl_verbose) erlc $(ERLC_OPTS) -o ebin/ ebin/*.erl
	@rm ebin/*.erl
endef

define compile_asn1
	$(asn1_verbose) erlc -v -I include/ -o ebin/ $(1)
	@mv ebin/*.hrl include/
	@mv ebin/*.asn1db include/
	@rm ebin/*.erl
endef

define compile_mib
	$(mib_verbose) erlc -v $(ERLC_MIB_OPTS) -o priv/mibs/ \
		-I priv/mibs/ $(COMPILE_MIB_FIRST_PATHS) $(1)
	$(mib_verbose) erlc -o include/ -- priv/mibs/*.bin
endef

ifneq ($(wildcard src/),)
ebin/$(PROJECT).app::
	@mkdir -p ebin/

ifneq ($(wildcard asn1/),)
ebin/$(PROJECT).app:: $(shell find asn1 -type f -name \*.asn1)
	@mkdir -p include
	$(if $(strip $?),$(call compile_asn1,$?))
endif

ifneq ($(wildcard mibs/),)
ebin/$(PROJECT).app:: $(shell find mibs -type f -name \*.mib)
	@mkdir -p priv/mibs/ include
	$(if $(strip $?),$(call compile_mib,$?))
endif

ebin/$(PROJECT).app:: $(shell find src -type f -name \*.erl -o -name \*.core)
	$(if $(strip $?),$(call compile_erl,$?))

ebin/$(PROJECT).app:: $(shell find src -type f -name \*.xrl -o -name \*.yrl)
	$(if $(strip $?),$(call compile_xyrl,$?))
endif

clean:: clean-app

clean-app:
	$(gen_verbose) rm -rf ebin/ priv/mibs/ \
		$(addprefix include/,$(addsuffix .hrl,$(notdir $(basename $(wildcard mibs/*.mib)))))
