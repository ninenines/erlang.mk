# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-edoc edoc

# Configuration.

EDOC_OPTS ?=
EDOC_SRC_DIRS ?=
EDOC_OUTPUT ?= doc

define edoc.erl
	SrcPaths = lists:foldl(fun(P, Acc) ->
		filelib:wildcard(atom_to_list(P) ++ "/{src,c_src}") ++ Acc
	end, [], [$(call comma_list,$(patsubst %,'%',$(EDOC_SRC_DIRS)))]),
	DefaultOpts = [{dir, "$(EDOC_OUTPUT)"}, {source_path, SrcPaths}, {subpackages, false}],
	edoc:application($(1), ".", [$(2)] ++ DefaultOpts),
	halt(0).
endef

# Core targets.

ifneq ($(strip $(EDOC_SRC_DIRS)$(wildcard doc/overview.edoc)),)
docs:: edoc
endif

distclean:: distclean-edoc

# Plugin-specific targets.

edoc: distclean-edoc doc-deps
	$(gen_verbose) $(call erlang,$(call edoc.erl,$(PROJECT),$(EDOC_OPTS)))

distclean-edoc:
	$(gen_verbose) rm -f $(EDOC_OUTPUT)/*.css $(EDOC_OUTPUT)/*.html $(EDOC_OUTPUT)/*.png $(EDOC_OUTPUT)/edoc-info
