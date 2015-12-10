# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-edoc edoc

# Configuration.

EDOC_OPTS ?=
EDOC_SRC_DIRS ?= $(ALL_APPS_DIRS) $(ALL_DEPS_DIRS)

# TODO: use double-quote instead of single + atom_to_list
#       The problem is in correctly escaping double-quotes
define edoc.erl
	SrcPaths = lists:foldl(fun (P, Acc) ->
	                         filelib:wildcard(atom_to_list(P) ++ "/{src,c_src}") ++ Acc
	                       end, [], [$(call comma_list,$(patsubst %,'%',$(EDOC_SRC_DIRS)))]),
	DefaultOpts = [ {source_path, SrcPaths}
	               ,{subpackages, false} ],
	edoc:application($(1), ".", [$(2)] ++ DefaultOpts),
	halt(0).
endef

# Core targets.
docs:: distclean-edoc edoc

distclean:: distclean-edoc

# Plugin-specific targets.

edoc: doc-deps
	$(gen_verbose) $(call erlang,$(call edoc.erl,$(PROJECT),$(EDOC_OPTS)))

distclean-edoc:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info
