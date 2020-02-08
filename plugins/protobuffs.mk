# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Verbosity.

proto_verbose_0 = @echo " PROTO " $(filter %.proto,$(?F));
proto_verbose = $(proto_verbose_$(V))

# Core targets.

ifneq ($(wildcard src/),)
ifneq ($(filter gpb protobuffs,$(BUILD_DEPS) $(DEPS)),)
PROTO_FILES := $(filter %.proto,$(ALL_SRC_FILES))
ERL_FILES += $(addprefix src/,$(patsubst %.proto,%_pb.erl,$(notdir $(PROTO_FILES))))

ifeq ($(PROTO_FILES),)
$(ERLANG_MK_TMP)/last-makefile-change-protobuffs:
	$(verbose) :
else
# Rebuild proto files when the Makefile changes.
# We exclude $(PROJECT).d to avoid a circular dependency.
$(ERLANG_MK_TMP)/last-makefile-change-protobuffs: $(filter-out $(PROJECT).d,$(MAKEFILE_LIST)) | $(ERLANG_MK_TMP)
	$(verbose) if test -f $@; then \
		touch $(PROTO_FILES); \
	fi
	$(verbose) touch $@

$(PROJECT).d:: $(ERLANG_MK_TMP)/last-makefile-change-protobuffs
endif

ifeq ($(filter gpb,$(BUILD_DEPS) $(DEPS)),)
define compile_proto.erl
	[begin
		protobuffs_compile:generate_source(F, [
			{output_include_dir, "./include"},
			{output_src_dir, "./src"}])
	end || F <- string:tokens("$1", " ")],
	halt().
endef
else
define compile_proto.erl
	[begin
		gpb_compile:file(F, [
			{include_as_lib, true},
			{module_name_suffix, "_pb"},
			{o_hrl, "./include"},
			{o_erl, "./src"}])
	end || F <- string:tokens("$1", " ")],
	halt().
endef
endif

ifneq ($(PROTO_FILES),)
$(PROJECT).d:: $(PROTO_FILES)
	$(verbose) mkdir -p ebin/ include/
	$(if $(strip $?),$(proto_verbose) $(call erlang,$(call compile_proto.erl,$?)))
endif
endif
endif
