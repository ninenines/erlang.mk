# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifeq ($(findstring erlydtl,$(ERLANG_MK_DISABLE_PLUGINS)),)

# Configuration.

DTL_FULL_PATH ?= 0
DTL_PATH ?= templates/
DTL_SUFFIX ?= _dtl

# Verbosity.

dtl_verbose_0 = @echo " DTL   " $(filter %.dtl,$(?F));
dtl_verbose = $(dtl_verbose_$(V))

# Core targets.

define erlydtl_compile.erl
	[begin
		Module0 = case $(DTL_FULL_PATH) of
			0 ->
				filename:basename(F, ".dtl");
			1 ->
				"$(DTL_PATH)" ++ F2 = filename:rootname(F, ".dtl"),
				re:replace(F2, "/",  "_",  [{return, list}, global])
		end,
		Module = list_to_atom(string:to_lower(Module0) ++ "$(DTL_SUFFIX)"),
		case erlydtl:compile(F, Module, [{out_dir, "ebin/"}, return_errors, {doc_root, "templates"}]) of
			ok -> ok;
			{ok, _} -> ok
		end
	end || F <- string:tokens("$(1)", " ")],
	halt().
endef

ifneq ($(wildcard src/),)
ebin/$(PROJECT).app:: $(sort $(call core_find,$(DTL_PATH),*.dtl))
	$(if $(strip $?),\
		$(dtl_verbose) $(call erlang,$(call erlydtl_compile.erl,$?,-pa ebin/ $(DEPS_DIR)/erlydtl/ebin/)))
endif

endif # ERLANG_MK_DISABLE_PLUGINS
