# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2015, Erlang Solutions Ltd.
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: xref distclean-xref

# Configuration.

ifeq ($(XREF_CONFIG),)
	XREFR_ARGS :=
else
	XREFR_ARGS := -c $(XREF_CONFIG)
endif

XREFR ?= $(CURDIR)/xrefr
export XREFR

XREFR_URL ?= https://github.com/inaka/xref_runner/releases/download/1.1.0/xrefr

# Core targets.

help::
	$(verbose) printf '%s\n' '' \
		'Xref targets:' \
		'  xref        Run Xrefr using $$XREF_CONFIG as config file if defined'

distclean:: distclean-xref

# Plugin-specific targets.

$(XREFR):
	$(gen_verbose) $(call core_http_get,$(XREFR),$(XREFR_URL))
	$(verbose) chmod +x $(XREFR)

xref: deps app $(XREFR)
	$(gen_verbose) $(XREFR) $(XREFR_ARGS)

distclean-xref:
	$(gen_verbose) rm -rf $(XREFR)
