# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: rel-deps

# Configuration.

ALL_REL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(REL_DEPS))

# Targets.

$(foreach dep,$(REL_DEPS),$(eval $(call dep_target,$(dep))))

ifneq ($(SKIP_DEPS),)
rel-deps:
else
rel-deps: $(ALL_REL_DEPS_DIRS)
	$(verbose) set -e; for dep in $(ALL_REL_DEPS_DIRS) ; do $(MAKE) -C $$dep; done
endif
