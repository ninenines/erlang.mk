# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifeq ($(findstring protobuffs,$(ERLANG_MK_DISABLE_PLUGINS)),)

# Verbosity.

proto_verbose_0 = @echo " PROTO " $(filter %.proto,$(?F));
proto_verbose = $(proto_verbose_$(V))

# Core targets.

define compile_proto
	$(verbose) mkdir -p ebin/ include/
	$(proto_verbose) $(call erlang,$(call compile_proto.erl,$(1)))
	$(proto_verbose) erlc +debug_info -o ebin/ ebin/*.erl
	$(verbose) rm ebin/*.erl
endef

define compile_proto.erl
	[begin
		Dir = filename:dirname(filename:dirname(F)),
		protobuffs_compile:generate_source(F,
			[{output_include_dir, Dir ++ "/include"},
				{output_src_dir, Dir ++ "/ebin"}])
	end || F <- string:tokens("$(1)", " ")],
	halt().
endef

ifneq ($(wildcard src/),)
ebin/$(PROJECT).app:: $(sort $(call core_find,src/,*.proto))
	$(if $(strip $?),$(call compile_proto,$?))
endif

endif # ERLANG_MK_DISABLE_PLUGINS
