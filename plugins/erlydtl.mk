# Copyright (c) Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Configuration.

DTL_FULL_PATH ?=
DTL_PATH ?= templates/
DTL_PREFIX ?=
DTL_SUFFIX ?= _dtl
DTL_OPTS ?=

# Verbosity.

dtl_verbose_0 = @echo " DTL   " $(filter %.dtl,$(?F));
dtl_verbose = $(dtl_verbose_$(V))

# Core targets.

DTL_PATH := $(abspath $(DTL_PATH))
DTL_FILES := $(sort $(call core_find,$(DTL_PATH),*.dtl))

ifneq ($(DTL_FILES),)

DTL_NAMES   = $(addprefix $(DTL_PREFIX),$(addsuffix $(DTL_SUFFIX),$(DTL_FILES:$(subst %,\%,$(DTL_PATH))/%.dtl=%)))
DTL_MODULES = $(if $(DTL_FULL_PATH),$(subst /,_,$(DTL_NAMES)),$(notdir $(DTL_NAMES)))
BEAM_FILES += $(addsuffix .beam,$(addprefix ebin/,$(DTL_MODULES)))

ifneq ($(words $(DTL_FILES)),0)
# Rebuild templates when a Makefile changes, without touching them.
# $(PROJECT).d is excluded to avoid a circular dependency.
$(ERLANG_MK_TMP)/last-makefile-change-erlydtl: $(filter-out $(PROJECT).d,$(MAKEFILE_LIST)) | $(ERLANG_MK_TMP)
	$(verbose) touch $@
endif

define erlydtl_compile.erl
	[begin
		Module0 = case "$(strip $(DTL_FULL_PATH))" of
			"" ->
				filename:basename(F, ".dtl");
			_ ->
				"$(call core_native_path,$(DTL_PATH))/" ++ F2 = filename:rootname(F, ".dtl"),
				re:replace(F2, "/",  "_",  [{return, list}, global])
		end,
		Module = list_to_atom("$(DTL_PREFIX)" ++ string:to_lower(Module0) ++ "$(DTL_SUFFIX)"),
		case erlydtl:compile(F, Module, [$(DTL_OPTS)] ++ [$(2)] ++ [{out_dir, "ebin/"}, return_errors]) of
			ok -> ok;
			{ok, _} -> ok;
			{error, Errors, Warnings} ->
				io:format("Errors: ~p~nWarnings: ~p~n", [Errors, Warnings]),
				halt(91)
		end
	end || F <- string:tokens("$(1)", " ")],
	halt().
endef

define erlydtl_compile_one
	@echo " DTL   " $(notdir $1);
	$(verbose) $(call erlang,$(call erlydtl_compile.erl,$(call core_native_path,$1),force_recompile),-pa ebin/)

endef

ebin/$(PROJECT).app:: $(DTL_FILES) $(ERLANG_MK_TMP)/last-makefile-change-erlydtl | ebin/
	$(if $(filter $(ERLANG_MK_TMP)/last-makefile-change-erlydtl,$?),\
		$(foreach f,$(DTL_FILES),$(call erlydtl_compile_one,$f)),\
		$(if $(strip $(filter %.dtl,$?)),$(dtl_verbose) $(call erlang,$(call erlydtl_compile.erl,$(call core_native_path,$(filter %.dtl,$?))),-pa ebin/)))

endif
