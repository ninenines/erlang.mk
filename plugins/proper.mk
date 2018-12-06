# Copyright (c) 2015-2017, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifeq ($(filter proper,$(DEPS) $(TEST_DEPS)),proper)
.PHONY: proper

# Targets.

tests:: proper

define proper_check.erl
	$(call cover.erl)
	code:add_pathsa([
		"$(call core_native_path,$(CURDIR)/ebin)",
		"$(call core_native_path,$(DEPS_DIR)/*/ebin)",
		"$(call core_native_path,$(TEST_DIR))"]),
	Module = fun(M) ->
		[true] =:= lists:usort([
			case atom_to_list(F) of
				"prop_" ++ _ ->
					io:format("Testing ~p:~p/0~n", [M, F]),
					proper:quickcheck(M:F(), nocolors);
				_ ->
					true
			end
		|| {F, 0} <- M:module_info(exports)])
	end,
	try begin
		CoverSetup(),
		Res = case $(1) of
			all -> [true] =:= lists:usort([Module(M) || M <- [$(call comma_list,$(3))]]);
			module -> Module($(2));
			function -> proper:quickcheck($(2), nocolors)
		end,
		CoverExport("$(COVER_DATA_DIR)/proper.coverdata"),
		Res
	end of
		true -> halt(0);
		_ -> halt(1)
	catch error:undef ->
		io:format("Undefined property or module?~n~p~n", [erlang:get_stacktrace()]),
		halt(0)
	end.
endef

ifdef t
ifeq (,$(findstring :,$(t)))
proper: test-build cover-data-dir
	$(verbose) $(call erlang,$(call proper_check.erl,module,$(t)))
else
proper: test-build cover-data-dir
	$(verbose) echo Testing $(t)/0
	$(verbose) $(call erlang,$(call proper_check.erl,function,$(t)()))
endif
else
proper: test-build cover-data-dir
	$(eval MODULES := $(patsubst %,'%',$(sort $(notdir $(basename \
		$(wildcard ebin/*.beam) $(call core_find,$(TEST_DIR)/,*.beam))))))
	$(gen_verbose) $(call erlang,$(call proper_check.erl,all,undefined,$(MODULES)))
endif
endif
