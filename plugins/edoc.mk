# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-edoc

# Configuration.

EDOC_OPTS ?=

# Core targets.

docs:: distclean-edoc
	$(gen_verbose) erl -noshell \
		-eval 'edoc:application($(PROJECT), ".", [$(EDOC_OPTS)]), init:stop().'

distclean:: distclean-edoc

# Plugin-specific targets.

distclean-edoc:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info
