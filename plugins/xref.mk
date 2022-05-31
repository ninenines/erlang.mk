# Copyright (c) 2022, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: xref

# Configuration.

# We do not use locals_not_used or deprecated_function_calls
# because the compiler will error out by default in those
# cases with Erlang.mk. Deprecated functions may make sense
# in some cases but few libraries define them. We do not
# use exports_not_used by default because it hinders more
# than it helps library projects such as Cowboy. Finally,
# undefined_functions provides little that undefined_function_calls
# doesn't already provide, so it's not enabled by default.
XREF_CHECKS ?= [undefined_function_calls]

# Instead of predefined checks a query can be evaluated
# using the Xref DSL. The $q variable is used in that case.

# The scope is a list of keywords that correspond to
# application directories, being essentially an easy way
# to configure which applications to analyze. With:
#
# - app:  .
# - apps: $(ALL_APPS_DIRS)
# - deps: $(ALL_DEPS_DIRS)
# - otp:  Built-in Erlang/OTP applications.
#
# The default is conservative (app) and will not be
# appropriate for all types of queries (for example
# application_call requires adding all applications
# that might be called or they will not be found).
XREF_SCOPE ?= app # apps deps otp

# If the above is not enough, additional application
# directories can be configured.
XREF_EXTRA_APP_DIRS ?=

# As well as additional non-application directories.
XREF_EXTRA_DIRS ?=

# Erlang.mk supports -ignore_xref([...]) with forms
# {M, F, A} | {F, A} | M, the latter ignoring whole
# modules. Ignores can also be provided project-wide.
XREF_IGNORE ?= []

# All callbacks may be ignored. Erlang.mk will ignore
# them automatically for exports_not_used (unless it
# is explicitly disabled by the user).
XREF_IGNORE_CALLBACKS ?=

# Core targets.

help::
	$(verbose) printf '%s\n' '' \
		'Xref targets:' \
		'  xref         Analyze the project using Xref' \
		'  xref q=QUERY Evaluate an Xref query'

# Plugin-specific targets.

define xref.erl
	{ok, Xref} = xref:start([]),
	Scope = [$(call comma_list,$(XREF_SCOPE))],
	AppDirs0 = [$(call comma_list,$(foreach d,$(XREF_EXTRA_APP_DIRS),"$d"))],
	AppDirs1 = case lists:member(otp, Scope) of
		false -> AppDirs0;
		true ->
			RootDir = code:root_dir(),
			AppDirs0 ++ [filename:dirname(P) || P <- code:get_path(), lists:prefix(RootDir, P)]
	end,
	AppDirs2 = case lists:member(deps, Scope) of
		false -> AppDirs1;
		true -> [$(call comma_list,$(foreach d,$(ALL_DEPS_DIRS),"$d"))] ++ AppDirs1
	end,
	AppDirs3 = case lists:member(apps, Scope) of
		false -> AppDirs2;
		true -> [$(call comma_list,$(foreach d,$(ALL_APPS_DIRS),"$d"))] ++ AppDirs2
	end,
	AppDirs = case lists:member(app, Scope) of
		false -> AppDirs3;
		true -> ["../$(notdir $(CURDIR))"|AppDirs3]
	end,
	[{ok, _} = xref:add_application(Xref, AppDir, [{builtins, true}]) || AppDir <- AppDirs],
	ExtraDirs = [$(call comma_list,$(foreach d,$(XREF_EXTRA_DIRS),"$d"))],
	[{ok, _} = xref:add_directory(Xref, ExtraDir, [{builtins, true}]) || ExtraDir <- ExtraDirs],
	ok = xref:set_library_path(Xref, code:get_path() -- (["ebin", "."] ++ AppDirs ++ ExtraDirs)),
	Checks = case {$1, is_list($2)} of
		{check, true} -> $2;
		{check, false} -> [$2];
		{query, _} -> [$2]
	end,
	FinalRes = [begin
		IsInformational = case $1 of
			query -> true;
			check ->
				is_tuple(Check) andalso
					lists:member(element(1, Check),
						[call, use, module_call, module_use, application_call, application_use])
		end,
		{ok, Res0} = case $1 of
			check -> xref:analyze(Xref, Check);
			query -> xref:q(Xref, Check)
		end,
		Res = case IsInformational of
			true -> Res0;
			false ->
				lists:filter(fun(R) ->
					{Mod, MFA} = case R of
						{MFA0 = {M, _, _}, _} -> {M, MFA0};
						{M, _, _} -> {M, R}
					end,
					Attrs = try
						Mod:module_info(attributes)
					catch error:undef ->
						[]
					end,
					InlineIgnores = lists:flatten([
						[case V of
							M when is_atom(M) -> {M, '_', '_'};
							{F, A} -> {Mod, F, A};
							_ -> V
						end || V <- Values]
					|| {ignore_xref, Values} <- Attrs]),
					BuiltinIgnores = [
						{eunit_test, wrapper_test_exported_, 0}
					],
					DoCallbackIgnores = case {Check, "$(strip $(XREF_IGNORE_CALLBACKS))"} of
						{exports_not_used, ""} -> true;
						{_, "0"} -> false;
						_ -> true
					end,
					CallbackIgnores = case DoCallbackIgnores of
						false -> [];
						true ->
							Behaviors = lists:flatten([
								[BL || {behavior, BL} <- Attrs],
								[BL || {behaviour, BL} <- Attrs]
							]),
							[{Mod, CF, CA} || B <- Behaviors, {CF, CA} <- B:behaviour_info(callbacks)]
					end,
					WideIgnores = if
						is_list($(XREF_IGNORE)) ->
							[if is_atom(I) -> {I, '_', '_'}; true -> I end
								|| I <- $(XREF_IGNORE)];
						true -> [$(XREF_IGNORE)]
					end,
					Ignores = InlineIgnores ++ BuiltinIgnores ++ CallbackIgnores ++ WideIgnores,
					not (lists:member(MFA, Ignores)
					orelse lists:member({Mod, '_', '_'}, Ignores))
				end, Res0)
		end,
		case Res of
			[] -> ok;
			_ when IsInformational ->
				case Check of
					{call, {CM, CF, CA}} ->
						io:format("Functions that ~s:~s/~b calls:~n", [CM, CF, CA]);
					{use, {CM, CF, CA}} ->
						io:format("Function ~s:~s/~b is called by:~n", [CM, CF, CA]);
					{module_call, CMod} ->
						io:format("Modules that ~s calls:~n", [CMod]);
					{module_use, CMod} ->
						io:format("Module ~s is used by:~n", [CMod]);
					{application_call, CApp} ->
						io:format("Applications that ~s calls:~n", [CApp]);
					{application_use, CApp} ->
						io:format("Application ~s is used by:~n", [CApp]);
					_ when $1 =:= query ->
						io:format("Query ~s returned:~n", [Check])
				end,
				[case R of
					{{InM, InF, InA}, {M, F, A}} ->
						io:format("- ~s:~s/~b called by ~s:~s/~b~n",
							[M, F, A, InM, InF, InA]);
					{M, F, A} ->
						io:format("- ~s:~s/~b~n", [M, F, A]);
					ModOrApp ->
						io:format("- ~s~n", [ModOrApp])
				end || R <- Res],
				ok;
			_ ->
				[case {Check, R} of
					{undefined_function_calls, {{InM, InF, InA}, {M, F, A}}} ->
						io:format("Undefined function ~s:~s/~b called by ~s:~s/~b~n",
							[M, F, A, InM, InF, InA]);
					{undefined_functions, {M, F, A}} ->
						io:format("Undefined function ~s:~s/~b~n", [M, F, A]);
					{locals_not_used, {M, F, A}} ->
						io:format("Unused local function ~s:~s/~b~n", [M, F, A]);
					{exports_not_used, {M, F, A}} ->
						io:format("Unused exported function ~s:~s/~b~n", [M, F, A]);
					{deprecated_function_calls, {{InM, InF, InA}, {M, F, A}}} ->
						io:format("Deprecated function ~s:~s/~b called by ~s:~s/~b~n",
							[M, F, A, InM, InF, InA]);
					{deprecated_functions, {M, F, A}} ->
						io:format("Deprecated function ~s:~s/~b~n", [M, F, A]);
					_ ->
						io:format("~p: ~p~n", [Check, R])
				end || R <- Res],
				error
		end
	end || Check <- Checks],
	stopped = xref:stop(Xref),
	case lists:usort(FinalRes) of
		[ok] -> halt(0);
		_ -> halt(1)
	end
endef

xref: deps app
ifdef q
	$(verbose) $(call erlang,$(call xref.erl,query,"$q"),-pa ebin/)
else
	$(verbose) $(call erlang,$(call xref.erl,check,$(XREF_CHECKS)),-pa ebin/)
endif
