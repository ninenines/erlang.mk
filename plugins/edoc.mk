# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2015, Viktor Söderqvist <viktor@zuiderkwast.se>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-edoc build-doc-deps

# Configuration.

EDOC_OPTS ?=

# Core targets.

docs:: distclean-edoc build-doc-deps
	$(gen_verbose) $(ERL) -eval 'edoc:application($(PROJECT), ".", [$(EDOC_OPTS)]), halt().'

distclean:: distclean-edoc

# Plugin-specific targets.

DOC_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(DOC_DEPS))

$(foreach dep,$(DOC_DEPS),$(eval $(call dep_target,$(dep))))

build-doc-deps: $(DOC_DEPS_DIRS)
	@for dep in $(DOC_DEPS_DIRS) ; do $(MAKE) -C $$dep; done

distclean-edoc:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info
