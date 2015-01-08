# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Configuration.

DTL_FULL_PATH ?= 0

# Verbosity.

dtl_verbose_0 = @echo " DTL   " $(filter %.dtl,$(?F));
dtl_verbose = $(dtl_verbose_$(V))

# Core targets.

define compile_erlydtl
	$(dtl_verbose) $(ERL) -pa ebin/ $(DEPS_DIR)/erlydtl/ebin/ -eval ' \
		Compile = fun(F) -> \
			S = fun (1) -> re:replace(filename:rootname(string:sub_string(F, 11), ".dtl"), "/",  "_",  [{return, list}, global]); \
				(0) -> filename:basename(F, ".dtl") \
			end, \
			Module = list_to_atom(string:to_lower(S($(DTL_FULL_PATH))) ++ "_dtl"), \
			{ok, _} = erlydtl:compile(F, Module, [{out_dir, "ebin/"}, return_errors, {doc_root, "templates"}]) \
		end, \
		_ = [Compile(F) || F <- string:tokens("$(1)", " ")], \
		halt().'
endef

ifneq ($(wildcard src/),)
ebin/$(PROJECT).app:: $(shell find templates -type f -name \*.dtl 2>/dev/null)
	$(if $(strip $?),$(call compile_erlydtl,$?))
endif
