# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifneq ($(wildcard $(DEPS_DIR)/triq),)
.PHONY: triq

# Targets.

tests:: triq

define triq_run
$(ERL) -pa $(CURDIR)/ebin $(DEPS_DIR)/*/ebin \
	-eval "try $(1) of true -> halt(0); _ -> halt(1) catch error:undef -> io:format(\"Undefined property or module~n\"), halt() end."
endef

ifdef t
ifeq (,$(findstring :,$(t)))
triq: test-build
	@$(call triq_run,triq:check($(t)))
else
triq: test-build
	@echo Testing $(t)/0
	@$(call triq_run,triq:check($(t)()))
endif
else
triq: test-build
	$(eval MODULES := $(shell find ebin -type f -name \*.beam \
		| sed "s/ebin\//'/;s/\.beam/',/" | sed '$$s/.$$//'))
	$(gen_verbose) $(call triq_run,[true] =:= lists:usort([triq:check(M) || M <- [$(MODULES)]]))
endif
endif
