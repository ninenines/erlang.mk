# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-rel

# Configuration.

RELX_CONFIG ?= $(CURDIR)/relx.config

ifneq ($(wildcard $(RELX_CONFIG)),)

RELX ?= $(CURDIR)/relx
export RELX

RELX_URL ?= https://github.com/erlware/relx/releases/download/v1.0.2/relx
RELX_OPTS ?=
RELX_OUTPUT_DIR ?= _rel

ifneq ($(firstword $(subst -o,,$(RELX_OPTS))),)
	RELX_OUTPUT_DIR = $(firstword $(subst -o,,$(RELX_OPTS)))
endif

# Core targets.

rel:: distclean-rel $(RELX)
	@$(RELX) -c $(RELX_CONFIG) $(RELX_OPTS)

distclean:: distclean-rel

# Plugin-specific targets.

define relx_fetch
	$(call core_http_get,$(RELX),$(RELX_URL))
	chmod +x $(RELX)
endef

$(RELX):
	@$(call relx_fetch)

distclean-rel:
	$(gen_verbose) rm -rf $(RELX_OUTPUT_DIR)

endif
