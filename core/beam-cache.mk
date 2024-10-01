# Copyright (c) 2024, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: beam-cache-restore-app beam-cache-restore-test clean-beam-cache distclean-beam-cache

BEAM_CACHE_DIR ?= $(ERLANG_MK_TMP)/beam-cache
PROJECT_BEAM_CACHE_DIR = $(BEAM_CACHE_DIR)/$(PROJECT)

clean:: clean-beam-cache

clean-beam-cache:
	$(verbose) rm -rf $(PROJECT_BEAM_CACHE_DIR)

distclean:: distclean-beam-cache

$(PROJECT_BEAM_CACHE_DIR):
	$(verbose) mkdir -p $(PROJECT_BEAM_CACHE_DIR)

distclean-beam-cache:
	$(gen_verbose) rm -rf $(BEAM_CACHE_DIR)

beam-cache-restore-app: | $(PROJECT_BEAM_CACHE_DIR)
	$(verbose) rm -rf $(PROJECT_BEAM_CACHE_DIR)/ebin-test
ifneq ($(wildcard ebin/),)
	$(verbose) mv ebin/ $(PROJECT_BEAM_CACHE_DIR)/ebin-test
endif
ifneq ($(wildcard $(PROJECT_BEAM_CACHE_DIR)/ebin-app),)
	$(gen_verbose) mv $(PROJECT_BEAM_CACHE_DIR)/ebin-app ebin/
else
	$(verbose) $(MAKE) --no-print-directory clean-app
endif

beam-cache-restore-test: | $(PROJECT_BEAM_CACHE_DIR)
	$(verbose) rm -rf $(PROJECT_BEAM_CACHE_DIR)/ebin-app
ifneq ($(wildcard ebin/),)
	$(verbose) mv ebin/ $(PROJECT_BEAM_CACHE_DIR)/ebin-app
endif
ifneq ($(wildcard $(PROJECT_BEAM_CACHE_DIR)/ebin-test),)
	$(gen_verbose) mv $(PROJECT_BEAM_CACHE_DIR)/ebin-test ebin/
else
	$(verbose) $(MAKE) --no-print-directory clean-app
endif
