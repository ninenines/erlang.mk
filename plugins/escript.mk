# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2014, Dave Cottlehuber <dch@skunkwerks.at>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-escript escript escript-zip

# Configuration.

ESCRIPT_NAME ?= $(PROJECT)
ESCRIPT_FILE ?= $(ESCRIPT_NAME)

ESCRIPT_SHEBANG ?= /usr/bin/env escript
ESCRIPT_COMMENT ?= This is an -*- erlang -*- file
ESCRIPT_EMU_ARGS ?= -escript main $(ESCRIPT_NAME)

ESCRIPT_ZIP ?= 7z a -tzip -mx=9 -mtc=off $(if $(filter-out 0,$(V)),,> /dev/null)
ESCRIPT_ZIP_FILE ?= $(ERLANG_MK_TMP)/escript.zip

# Core targets.

distclean:: distclean-escript

help::
	$(verbose) printf "%s\n" "" \
		"Escript targets:" \
		"  escript     Build an executable escript archive" \

# Plugin-specific targets.

escript-zip:: deps app
	$(verbose) mkdir -p $(dir $(ESCRIPT_ZIP))
	$(verbose) rm -f $(ESCRIPT_ZIP_FILE)
	$(gen_verbose) cd .. && $(ESCRIPT_ZIP) $(ESCRIPT_ZIP_FILE) $(PROJECT)/ebin/*
ifneq ($(DEPS),)
	$(verbose) cd $(DEPS_DIR) && $(ESCRIPT_ZIP) $(ESCRIPT_ZIP_FILE) \
		`cat $(ERLANG_MK_TMP)/deps.log | sed 's/^$(subst /,\/,$(DEPS_DIR))\///' | sed 's/$$/\/ebin\/\*/'`
endif

escript:: escript-zip
	$(gen_verbose) printf "%s\n" \
		"#!$(ESCRIPT_SHEBANG)" \
		"%% $(ESCRIPT_COMMENT)" \
		"%%! $(ESCRIPT_EMU_ARGS)" > $(ESCRIPT_FILE)
	$(verbose) cat $(ESCRIPT_ZIP_FILE) >> $(ESCRIPT_FILE)
	$(verbose) chmod +x $(ESCRIPT_FILE)

distclean-escript:
	$(gen_verbose) rm -f $(ESCRIPT_FILE)
