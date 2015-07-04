# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifneq ($(wildcard $(DEPS_DIR)/triq),)
.PHONY: triq

# Targets.

tests:: triq

define triq_check.erl
	code:add_pathsa(["$(CURDIR)/ebin", "$(DEPS_DIR)/*/ebin"]),
	try
		case $(1) of
			all -> [true] =:= lists:usort([triq:check(M) || M <- [$(call comma_list,$(3))]]);
			module -> triq:check($(2));
			function -> triq:check($(2))
		end
	of
		true -> halt(0);
		_ -> halt(1)
	catch error:undef ->
		io:format("Undefined property or module~n"),
		halt(0)
	end.
endef

ifdef t
ifeq (,$(findstring :,$(t)))
triq: test-build
	$(verbose) $(call erlang,$(call triq_check.erl,module,$(t)))
else
triq: test-build
	$(verbose) echo Testing $(t)/0
	$(verbose) $(call erlang,$(call triq_check.erl,function,$(t)()))
endif
else
triq: test-build
	$(eval MODULES := $(patsubst %,'%',$(sort $(notdir $(basename $(wildcard ebin/*.beam))))))
	$(gen_verbose) $(call erlang,$(call triq_check.erl,all,undefined,$(MODULES)))
endif
endif
