# Copyright (c) 2013-2017, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# External plugins.

DEP_PLUGINS ?=

define core_dep_plugin
-include $(DEPS_DIR)/$(1)

$(DEPS_DIR)/$(1): $(DEPS_DIR)/$(2) ;
endef

$(foreach p,$(DEP_PLUGINS),\
	$(eval $(if $(findstring /,$p),\
		$(call core_dep_plugin,$p,$(firstword $(subst /, ,$p))),\
		$(call core_dep_plugin,$p/plugins.mk,$p))))
