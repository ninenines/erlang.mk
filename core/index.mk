# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: search

define pkg_print
	$(verbose) printf "%s\n" \
		$(if $(call core_eq,$(1),$(pkg_$(1)_name)),,"Pkg name:    $(1)") \
		"App name:    $(pkg_$(1)_name)" \
		"Description: $(pkg_$(1)_description)" \
		"Home page:   $(pkg_$(1)_homepage)" \
		"Fetch with:  $(pkg_$(1)_fetch)" \
		"Repository:  $(pkg_$(1)_repo)" \
		"Commit:      $(pkg_$(1)_commit)" \
		""

endef

# If tag="..." used, first look at matching packages with tag(s) before using q="..."
ifdef tag
TE=$(addprefix -e , $(tag))
PACKAGESQ = $(shell grep $(TE) .erlang.mk/tags.index | cut -d ' ' -f 2- | tr " " "\n" | sort -u | tr " " "\n")
else
PACKAGESQ = $(PACKAGES)
endif

search:
ifdef q
	$(foreach p,$(PACKAGESQ), \
		$(if $(findstring $(call core_lc,$(q)),$(call core_lc,$(pkg_$(p)_name) $(pkg_$(p)_description))), \
			$(call pkg_print,$(p))))
else
	$(foreach p,$(PACKAGESQ),$(call pkg_print,$(p)))
endif

tags:
	$(verbose) printf "%s\n" $(shell cat .erlang.mk/tags.index | cut -d " " -f 1 )

