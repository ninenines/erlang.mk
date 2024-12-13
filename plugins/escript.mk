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

ALL_ESCRIPT_DEPS_DIRS = $(LOCAL_DEPS_DIRS) $(addprefix $(DEPS_DIR)/,$(foreach dep,$(filter-out $(IGNORE_DEPS),$(DEPS)),$(call query_name,$(dep))))

ESCRIPT_RUNTIME_DEPS_FILE ?= $(ERLANG_MK_TMP)/escript-deps.log

escript-list-runtime-deps:
ifeq ($(IS_DEP),)
	$(verbose) rm -f $(ESCRIPT_RUNTIME_DEPS_FILE)
endif
	$(verbose) touch $(ESCRIPT_RUNTIME_DEPS_FILE)
	$(verbose) set -e; for dep in $(ALL_ESCRIPT_DEPS_DIRS) ; do \
		if ! grep -qs ^$$dep$$ $(ESCRIPT_RUNTIME_DEPS_FILE); then \
			echo $$dep >> $(ESCRIPT_RUNTIME_DEPS_FILE); \
			if grep -qs -E "^[[:blank:]]*include[[:blank:]]+(erlang\.mk|.*/erlang\.mk|.*ERLANG_MK_FILENAME.*)$$" \
			 $$dep/GNUmakefile $$dep/makefile $$dep/Makefile; then \
				$(MAKE) -C $$dep escript-list-runtime-deps \
				 IS_DEP=1 \
				 ESCRIPT_RUNTIME_DEPS_FILE=$(ESCRIPT_RUNTIME_DEPS_FILE); \
			fi \
		fi \
	done
ifeq ($(IS_DEP),)
	$(verbose) sort < $(ESCRIPT_RUNTIME_DEPS_FILE) | uniq > $(ESCRIPT_RUNTIME_DEPS_FILE).sorted
	$(verbose) mv $(ESCRIPT_RUNTIME_DEPS_FILE).sorted $(ESCRIPT_RUNTIME_DEPS_FILE)
endif

escript-prepare: deps app
	$(MAKE) escript-list-runtime-deps

escript-zip:: escript-prepare
	$(verbose) mkdir -p $(dir $(abspath $(ESCRIPT_ZIP_FILE)))
	$(verbose) rm -f $(abspath $(ESCRIPT_ZIP_FILE))
	$(gen_verbose) cd .. && $(ESCRIPT_ZIP) $(abspath $(ESCRIPT_ZIP_FILE)) $(notdir $(CURDIR))/ebin/*
ifneq ($(DEPS),)
	$(verbose) cd $(DEPS_DIR) && $(ESCRIPT_ZIP) $(abspath $(ESCRIPT_ZIP_FILE)) \
		$(subst $(DEPS_DIR)/,,$(addsuffix /*,$(wildcard \
		$(addsuffix /ebin,$(shell cat $(ESCRIPT_RUNTIME_DEPS_FILE))))))
endif

escript:: escript-zip
	$(gen_verbose) printf "%s\n" \
		"#!$(ESCRIPT_SHEBANG)" \
		"%% $(ESCRIPT_COMMENT)" \
		"%%! $(ESCRIPT_EMU_ARGS)" > $(ESCRIPT_FILE)
	$(verbose) cat $(abspath $(ESCRIPT_ZIP_FILE)) >> $(ESCRIPT_FILE)
	$(verbose) chmod +x $(ESCRIPT_FILE)

distclean-escript:
	$(gen_verbose) rm -f $(ESCRIPT_FILE) $(abspath $(ESCRIPT_ZIP_FILE))
