# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-deps distclean-pkg pkg-list pkg-search

# Configuration.

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR

ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(DEPS))

ifeq ($(filter $(DEPS_DIR),$(subst :, ,$(ERL_LIBS))),)
ifeq ($(ERL_LIBS),)
	ERL_LIBS = $(DEPS_DIR)
else
	ERL_LIBS := $(ERL_LIBS):$(DEPS_DIR)
endif
endif
export ERL_LIBS

PKG_FILE ?= $(CURDIR)/.erlang.mk.packages.v2
export PKG_FILE

PKG_FILE_URL ?= https://raw.githubusercontent.com/extend/erlang.mk/master/packages.v2.tsv

# Core targets.

deps:: $(ALL_DEPS_DIRS)
	@for dep in $(ALL_DEPS_DIRS) ; do \
		if [ -f $$dep/Makefile ] ; then \
			$(MAKE) -C $$dep ; \
		else \
			echo "include $(CURDIR)/erlang.mk" | $(MAKE) -f - -C $$dep ; \
		fi ; \
	done

distclean:: distclean-deps distclean-pkg

# Deps related targets.

define dep_fetch_git
	git clone -n -- $(2) $(DEPS_DIR)/$(1)
	cd $(DEPS_DIR)/$(1) ; git checkout -q $(3)
endef

define dep_fetch
	@mkdir -p $(DEPS_DIR)
ifeq (,$(dep_$(1)))
	@if [ ! -f $(PKG_FILE) ]; then $(call core_http_get,$(PKG_FILE),$(PKG_FILE_URL)); fi
	$(eval DEP_FETCH_PKG := $(shell awk 'BEGIN { FS = "\t" }; $$1 == "$(1)" { print $$2 " " $$3 " "$$4 }' $(PKG_FILE)))
	$(call dep_fetch_$(word 1,$(DEP_FETCH_PKG)),$(1),$(word 2,$(DEP_FETCH_PKG)),$(word 3,$(DEP_FETCH_PKG)))
else
	$(call dep_fetch_$(word 1,$(dep_$(1))),$(1),$(word 2,$(dep_$(1))),$(word 3,$(dep_$(1))))
endif
endef

define dep_target
$(DEPS_DIR)/$(1):
	$(call dep_fetch,$(1))
endef

$(foreach dep,$(DEPS),$(eval $(call dep_target,$(dep))))

distclean-deps:
	$(gen_verbose) rm -rf $(DEPS_DIR)

# Packages related targets.

$(PKG_FILE):
	$(call core_http_get,$(PKG_FILE),$(PKG_FILE_URL))

pkg-list: $(PKG_FILE)
	@cat $(PKG_FILE) | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$3 "\n" \
		"Website:\t" $$5 "\n" \
		"Description:\t" $$6 "\n" }'

ifdef q
pkg-search: $(PKG_FILE)
	@cat $(PKG_FILE) | grep -i ${q} | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$3 "\n" \
		"Website:\t" $$5 "\n" \
		"Description:\t" $$6 "\n" }'
else
pkg-search:
	@echo "Usage: make pkg-search q=STRING"
endif

distclean-pkg:
	$(gen_verbose) rm -f $(PKG_FILE)

help::
	@printf "%s\n" "" \
		"Package-related targets:" \
		"  pkg-list              List all known packages" \
		"  pkg-search q=STRING   Search for STRING in the package index"
