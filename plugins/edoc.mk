# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-edoc edoc

# Configuration.

EDOC_OPTS ?=

# Core targets.

docs:: distclean-edoc edoc

distclean:: distclean-edoc

# Plugin-specific targets.

edoc: doc-deps
	$(gen_verbose) $(ERL) -eval 'edoc:application($(PROJECT), ".", [$(EDOC_OPTS)]), halt().'

distclean-edoc:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info
