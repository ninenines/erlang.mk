# Copyright (c) 2014, Juan Facorro <juan@inaka.net>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: elvis distclean-elvis

# Configuration.

ELVIS_CONFIG ?= $(CURDIR)/elvis.config

ELVIS ?= $(CURDIR)/elvis
export ELVIS

ELVIS_URL ?= https://github.com/inaka/elvis/releases/download/0.2.5-beta2/elvis
ELVIS_CONFIG_URL ?= https://github.com/inaka/elvis/releases/download/0.2.5-beta2/elvis.config
ELVIS_OPTS ?=

# Core targets.

help::
	@printf "%s\n" "" \
		"Elvis targets:" \
		"  elvis       Run Elvis using the local elvis.config or download the default otherwise"

distclean:: distclean-elvis

# Plugin-specific targets.

$(ELVIS):
	@$(call core_http_get,$(ELVIS),$(ELVIS_URL))
	@chmod +x $(ELVIS)

$(ELVIS_CONFIG):
	@$(call core_http_get,$(ELVIS_CONFIG),$(ELVIS_CONFIG_URL))

elvis: $(ELVIS) $(ELVIS_CONFIG)
	@$(ELVIS) rock -c $(ELVIS_CONFIG) $(ELVIS_OPTS)

distclean-elvis:
	$(gen_verbose) rm -rf $(ELVIS)
