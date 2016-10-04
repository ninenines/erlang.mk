# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-deps

# Configuration.

ifdef OTP_DEPS
$(warning The variable OTP_DEPS is deprecated in favor of LOCAL_DEPS.)
endif

IGNORE_DEPS ?=
export IGNORE_DEPS

APPS_DIR ?= $(CURDIR)/apps
export APPS_DIR

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR

dep_name = $(if $(dep_$(1)),$(1),$(if $(pkg_$(1)_name),$(pkg_$(1)_name),$(1)))
dep_repo = $(patsubst git://github.com/%,https://github.com/%, \
	$(if $(dep_$(1)),$(word 2,$(dep_$(1))),$(pkg_$(1)_repo)))
dep_commit = $(if $(dep_$(1)_commit),$(dep_$(1)_commit),$(if $(dep_$(1)),$(word 3,$(dep_$(1))),$(pkg_$(1)_commit)))

ALL_APPS_DIRS = $(if $(wildcard $(APPS_DIR)/),$(filter-out $(APPS_DIR),$(shell find $(APPS_DIR) -maxdepth 1 -type d)))
ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(foreach dep,$(filter-out $(IGNORE_DEPS),$(BUILD_DEPS) $(DEPS)),$(call dep_name,$(dep))))

ifeq ($(filter $(APPS_DIR) $(DEPS_DIR),$(subst :, ,$(ERL_LIBS))),)
ifeq ($(ERL_LIBS),)
	ERL_LIBS = $(APPS_DIR):$(DEPS_DIR)
else
	ERL_LIBS := $(ERL_LIBS):$(APPS_DIR):$(DEPS_DIR)
endif
endif
export ERL_LIBS

export NO_AUTOPATCH

# Verbosity.

dep_verbose_0 = @echo " DEP   " $(1);
dep_verbose_2 = set -x;
dep_verbose = $(dep_verbose_$(V))

# Core targets.

ifdef IS_APP
apps::
else
apps:: $(ALL_APPS_DIRS)
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) rm -f $(ERLANG_MK_TMP)/apps.log
endif
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
# Create ebin directory for all apps to make sure Erlang recognizes them
# as proper OTP applications when using -include_lib. This is a temporary
# fix, a proper fix would be to compile apps/* in the right order.
	$(verbose) for dep in $(ALL_APPS_DIRS) ; do \
		mkdir -p $$dep/ebin || exit $$?; \
	done
	$(verbose) for dep in $(ALL_APPS_DIRS) ; do \
		if grep -qs ^$$dep$$ $(ERLANG_MK_TMP)/apps.log; then \
			:; \
		else \
			echo $$dep >> $(ERLANG_MK_TMP)/apps.log; \
			$(MAKE) -C $$dep IS_APP=1 || exit $$?; \
		fi \
	done
endif

ifneq ($(SKIP_DEPS),)
deps::
else
deps:: $(ALL_DEPS_DIRS) apps
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) rm -f $(ERLANG_MK_TMP)/deps.log
endif
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(verbose) for dep in $(ALL_DEPS_DIRS) ; do \
		if grep -qs ^$$dep$$ $(ERLANG_MK_TMP)/deps.log; then \
			:; \
		else \
			echo $$dep >> $(ERLANG_MK_TMP)/deps.log; \
			if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ]; then \
				$(MAKE) -C $$dep IS_DEP=1 || exit $$?; \
			else \
				echo "Error: No Makefile to build dependency $$dep."; \
				exit 2; \
			fi \
		fi \
	done
endif

# Deps related targets.

# @todo rename GNUmakefile and makefile into Makefile first, if they exist
# While Makefile file could be GNUmakefile or makefile,
# in practice only Makefile is needed so far.
define dep_autopatch
	if [ -f $(DEPS_DIR)/$(1)/erlang.mk ]; then \
		$(call erlang,$(call dep_autopatch_appsrc.erl,$(1))); \
		$(call dep_autopatch_erlang_mk,$(1)); \
	elif [ -f $(DEPS_DIR)/$(1)/Makefile ]; then \
		if [ 0 != `grep -c "include ../\w*\.mk" $(DEPS_DIR)/$(1)/Makefile` ]; then \
			$(call dep_autopatch2,$(1)); \
		elif [ 0 != `grep -ci rebar $(DEPS_DIR)/$(1)/Makefile` ]; then \
			$(call dep_autopatch2,$(1)); \
		elif [ -n "`find $(DEPS_DIR)/$(1)/ -type f -name \*.mk -not -name erlang.mk -exec grep -i rebar '{}' \;`" ]; then \
			$(call dep_autopatch2,$(1)); \
		else \
			$(call erlang,$(call dep_autopatch_app.erl,$(1))); \
		fi \
	else \
		if [ ! -d $(DEPS_DIR)/$(1)/src/ ]; then \
			$(call dep_autopatch_noop,$(1)); \
		else \
			$(call dep_autopatch2,$(1)); \
		fi \
	fi
endef

define dep_autopatch2
	if [ -f $(DEPS_DIR)/$1/src/$1.app.src.script ]; then \
		$(call erlang,$(call dep_autopatch_appsrc_script.erl,$(1))); \
	fi; \
	$(call erlang,$(call dep_autopatch_appsrc.erl,$(1))); \
	if [ -f $(DEPS_DIR)/$(1)/rebar -o -f $(DEPS_DIR)/$(1)/rebar.config -o -f $(DEPS_DIR)/$(1)/rebar.config.script ]; then \
		$(call dep_autopatch_fetch_rebar); \
		$(call dep_autopatch_rebar,$(1)); \
	else \
		$(call dep_autopatch_gen,$(1)); \
	fi
endef

define dep_autopatch_noop
	printf "noop:\n" > $(DEPS_DIR)/$(1)/Makefile
endef

# Overwrite erlang.mk with the current file by default.
ifeq ($(NO_AUTOPATCH_ERLANG_MK),)
define dep_autopatch_erlang_mk
	echo "include $(call core_relpath,$(dir $(ERLANG_MK_FILENAME)),$(DEPS_DIR)/app)/erlang.mk" \
		> $(DEPS_DIR)/$1/erlang.mk
endef
else
define dep_autopatch_erlang_mk
	:
endef
endif

define dep_autopatch_gen
	printf "%s\n" \
		"ERLC_OPTS = +debug_info" \
		"include ../../erlang.mk" > $(DEPS_DIR)/$(1)/Makefile
endef

define dep_autopatch_fetch_rebar
	mkdir -p $(ERLANG_MK_TMP); \
	if [ ! -d $(ERLANG_MK_TMP)/rebar ]; then \
		git clone -q -n -- https://github.com/rebar/rebar $(ERLANG_MK_TMP)/rebar; \
		cd $(ERLANG_MK_TMP)/rebar; \
		git checkout -q 576e12171ab8d69b048b827b92aa65d067deea01; \
		$(MAKE); \
		cd -; \
	fi
endef

define dep_autopatch_rebar
	if [ -f $(DEPS_DIR)/$(1)/Makefile ]; then \
		mv $(DEPS_DIR)/$(1)/Makefile $(DEPS_DIR)/$(1)/Makefile.orig.mk; \
	fi; \
	$(call erlang,$(call dep_autopatch_rebar.erl,$(1))); \
	rm -f $(DEPS_DIR)/$(1)/ebin/$(1).app
endef

define dep_autopatch_rebar.erl
	application:load(rebar),
	application:set_env(rebar, log_level, debug),
	rmemo:start(),
	Conf1 = case file:consult("$(call core_native_path,$(DEPS_DIR)/$1/rebar.config)") of
		{ok, Conf0} -> Conf0;
		_ -> []
	end,
	{Conf, OsEnv} = fun() ->
		case filelib:is_file("$(call core_native_path,$(DEPS_DIR)/$1/rebar.config.script)") of
			false -> {Conf1, []};
			true ->
				Bindings0 = erl_eval:new_bindings(),
				Bindings1 = erl_eval:add_binding('CONFIG', Conf1, Bindings0),
				Bindings = erl_eval:add_binding('SCRIPT', "$(call core_native_path,$(DEPS_DIR)/$1/rebar.config.script)", Bindings1),
				Before = os:getenv(),
				{ok, Conf2} = file:script("$(call core_native_path,$(DEPS_DIR)/$1/rebar.config.script)", Bindings),
				{Conf2, lists:foldl(fun(E, Acc) -> lists:delete(E, Acc) end, os:getenv(), Before)}
		end
	end(),
	Write = fun (Text) ->
		file:write_file("$(call core_native_path,$(DEPS_DIR)/$1/Makefile)", Text, [append])
	end,
	Escape = fun (Text) ->
		re:replace(Text, "\\\\$$", "\$$$$", [global, {return, list}])
	end,
	Write("IGNORE_DEPS += edown eper eunit_formatters meck node_package "
		"rebar_lock_deps_plugin rebar_vsn_plugin reltool_util\n"),
	Write("C_SRC_DIR = /path/do/not/exist\n"),
	Write("C_SRC_TYPE = rebar\n"),
	Write("DRV_CFLAGS = -fPIC\nexport DRV_CFLAGS\n"),
	Write(["ERLANG_ARCH = ", rebar_utils:wordsize(), "\nexport ERLANG_ARCH\n"]),
	fun() ->
		Write("ERLC_OPTS = +debug_info\nexport ERLC_OPTS\n"),
		case lists:keyfind(erl_opts, 1, Conf) of
			false -> ok;
			{_, ErlOpts} ->
				lists:foreach(fun
					({d, D}) ->
						Write("ERLC_OPTS += -D" ++ atom_to_list(D) ++ "=1\n");
					({i, I}) ->
						Write(["ERLC_OPTS += -I ", I, "\n"]);
					({platform_define, Regex, D}) ->
						case rebar_utils:is_arch(Regex) of
							true -> Write("ERLC_OPTS += -D" ++ atom_to_list(D) ++ "=1\n");
							false -> ok
						end;
					({parse_transform, PT}) ->
						Write("ERLC_OPTS += +'{parse_transform, " ++ atom_to_list(PT) ++ "}'\n");
					(_) -> ok
				end, ErlOpts)
		end,
		Write("\n")
	end(),
	fun() ->
		File = case lists:keyfind(deps, 1, Conf) of
			false -> [];
			{_, Deps} ->
				[begin case case Dep of
							{N, S} when is_atom(N), is_list(S) -> {N, {hex, S}};
							{N, S} when is_tuple(S) -> {N, S};
							{N, _, S} -> {N, S};
							{N, _, S, _} -> {N, S};
							_ -> false
						end of
					false -> ok;
					{Name, Source} ->
						{Method, Repo, Commit} = case Source of
							{hex, V} -> {hex, V, undefined};
							{git, R} -> {git, R, master};
							{M, R, {branch, C}} -> {M, R, C};
							{M, R, {ref, C}} -> {M, R, C};
							{M, R, {tag, C}} -> {M, R, C};
							{M, R, C} -> {M, R, C}
						end,
						Write(io_lib:format("DEPS += ~s\ndep_~s = ~s ~s ~s~n", [Name, Name, Method, Repo, Commit]))
				end end || Dep <- Deps]
		end
	end(),
	fun() ->
		case lists:keyfind(erl_first_files, 1, Conf) of
			false -> ok;
			{_, Files} ->
				Names = [[" ", case lists:reverse(F) of
					"lre." ++ Elif -> lists:reverse(Elif);
					Elif -> lists:reverse(Elif)
				end] || "src/" ++ F <- Files],
				Write(io_lib:format("COMPILE_FIRST +=~s\n", [Names]))
		end
	end(),
	Write("\n\nrebar_dep: preprocess pre-deps deps pre-app app\n"),
	Write("\npreprocess::\n"),
	Write("\npre-deps::\n"),
	Write("\npre-app::\n"),
	PatchHook = fun(Cmd) ->
		case Cmd of
			"make -C" ++ Cmd1 -> "$$\(MAKE) -C" ++ Escape(Cmd1);
			"gmake -C" ++ Cmd1 -> "$$\(MAKE) -C" ++ Escape(Cmd1);
			"make " ++ Cmd1 -> "$$\(MAKE) -f Makefile.orig.mk " ++ Escape(Cmd1);
			"gmake " ++ Cmd1 -> "$$\(MAKE) -f Makefile.orig.mk " ++ Escape(Cmd1);
			_ -> Escape(Cmd)
		end
	end,
	fun() ->
		case lists:keyfind(pre_hooks, 1, Conf) of
			false -> ok;
			{_, Hooks} ->
				[case H of
					{'get-deps', Cmd} ->
						Write("\npre-deps::\n\t" ++ PatchHook(Cmd) ++ "\n");
					{compile, Cmd} ->
						Write("\npre-app::\n\tCC=$$\(CC) " ++ PatchHook(Cmd) ++ "\n");
					{Regex, compile, Cmd} ->
						case rebar_utils:is_arch(Regex) of
							true -> Write("\npre-app::\n\tCC=$$\(CC) " ++ PatchHook(Cmd) ++ "\n");
							false -> ok
						end;
					_ -> ok
				end || H <- Hooks]
		end
	end(),
	ShellToMk = fun(V) ->
		re:replace(re:replace(V, "(\\\\$$)(\\\\w*)", "\\\\1(\\\\2)", [global]),
			"-Werror\\\\b", "", [{return, list}, global])
	end,
	PortSpecs = fun() ->
		case lists:keyfind(port_specs, 1, Conf) of
			false ->
				case filelib:is_dir("$(call core_native_path,$(DEPS_DIR)/$1/c_src)") of
					false -> [];
					true ->
						[{"priv/" ++ proplists:get_value(so_name, Conf, "$(1)_drv.so"),
							proplists:get_value(port_sources, Conf, ["c_src/*.c"]), []}]
				end;
			{_, Specs} ->
				lists:flatten([case S of
					{Output, Input} -> {ShellToMk(Output), Input, []};
					{Regex, Output, Input} ->
						case rebar_utils:is_arch(Regex) of
							true -> {ShellToMk(Output), Input, []};
							false -> []
						end;
					{Regex, Output, Input, [{env, Env}]} ->
						case rebar_utils:is_arch(Regex) of
							true -> {ShellToMk(Output), Input, Env};
							false -> []
						end
				end || S <- Specs])
		end
	end(),
	PortSpecWrite = fun (Text) ->
		file:write_file("$(call core_native_path,$(DEPS_DIR)/$1/c_src/Makefile.erlang.mk)", Text, [append])
	end,
	case PortSpecs of
		[] -> ok;
		_ ->
			Write("\npre-app::\n\t$$\(MAKE) -f c_src/Makefile.erlang.mk\n"),
			PortSpecWrite(io_lib:format("ERL_CFLAGS = -finline-functions -Wall -fPIC -I \\"~s/erts-~s/include\\" -I \\"~s\\"\n",
				[code:root_dir(), erlang:system_info(version), code:lib_dir(erl_interface, include)])),
			PortSpecWrite(io_lib:format("ERL_LDFLAGS = -L \\"~s\\" -lerl_interface -lei\n",
				[code:lib_dir(erl_interface, lib)])),
			[PortSpecWrite(["\n", E, "\n"]) || E <- OsEnv],
			FilterEnv = fun(Env) ->
				lists:flatten([case E of
					{_, _} -> E;
					{Regex, K, V} ->
						case rebar_utils:is_arch(Regex) of
							true -> {K, V};
							false -> []
						end
				end || E <- Env])
			end,
			MergeEnv = fun(Env) ->
				lists:foldl(fun ({K, V}, Acc) ->
					case lists:keyfind(K, 1, Acc) of
						false -> [{K, rebar_utils:expand_env_variable(V, K, "")}|Acc];
						{_, V0} -> [{K, rebar_utils:expand_env_variable(V, K, V0)}|Acc]
					end
				end, [], Env)
			end,
			PortEnv = case lists:keyfind(port_env, 1, Conf) of
				false -> [];
				{_, PortEnv0} -> FilterEnv(PortEnv0)
			end,
			PortSpec = fun ({Output, Input0, Env}) ->
				filelib:ensure_dir("$(call core_native_path,$(DEPS_DIR)/$1/)" ++ Output),
				Input = [[" ", I] || I <- Input0],
				PortSpecWrite([
					[["\n", K, " = ", ShellToMk(V)] || {K, V} <- lists:reverse(MergeEnv(PortEnv))],
					case $(PLATFORM) of
						darwin -> "\n\nLDFLAGS += -flat_namespace -undefined suppress";
						_ -> ""
					end,
					"\n\nall:: ", Output, "\n\n",
					"%.o: %.c\n\t$$\(CC) -c -o $$\@ $$\< $$\(CFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					"%.o: %.C\n\t$$\(CXX) -c -o $$\@ $$\< $$\(CXXFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					"%.o: %.cc\n\t$$\(CXX) -c -o $$\@ $$\< $$\(CXXFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					"%.o: %.cpp\n\t$$\(CXX) -c -o $$\@ $$\< $$\(CXXFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					[[Output, ": ", K, " = ", ShellToMk(V), "\n"] || {K, V} <- lists:reverse(MergeEnv(FilterEnv(Env)))],
					Output, ": $$\(foreach ext,.c .C .cc .cpp,",
						"$$\(patsubst %$$\(ext),%.o,$$\(filter %$$\(ext),$$\(wildcard", Input, "))))\n",
					"\t$$\(CC) -o $$\@ $$\? $$\(LDFLAGS) $$\(ERL_LDFLAGS) $$\(DRV_LDFLAGS) $$\(EXE_LDFLAGS)",
					case {filename:extension(Output), $(PLATFORM)} of
					    {[], _} -> "\n";
					    {_, darwin} -> "\n";
					    _ -> " -shared\n"
					end])
			end,
			[PortSpec(S) || S <- PortSpecs]
	end,
	Write("\ninclude $(call core_relpath,$(dir $(ERLANG_MK_FILENAME)),$(DEPS_DIR)/app)/erlang.mk"),
	RunPlugin = fun(Plugin, Step) ->
		case erlang:function_exported(Plugin, Step, 2) of
			false -> ok;
			true ->
				c:cd("$(call core_native_path,$(DEPS_DIR)/$1/)"),
				Ret = Plugin:Step({config, "", Conf, dict:new(), dict:new(), dict:new(),
					dict:store(base_dir, "", dict:new())}, undefined),
				io:format("rebar plugin ~p step ~p ret ~p~n", [Plugin, Step, Ret])
		end
	end,
	fun() ->
		case lists:keyfind(plugins, 1, Conf) of
			false -> ok;
			{_, Plugins} ->
				[begin
					case lists:keyfind(deps, 1, Conf) of
						false -> ok;
						{_, Deps} ->
							case lists:keyfind(P, 1, Deps) of
								false -> ok;
								_ ->
									Path = "$(call core_native_path,$(DEPS_DIR)/)" ++ atom_to_list(P),
									io:format("~s", [os:cmd("$(MAKE) -C $(call core_native_path,$(DEPS_DIR)/$1) " ++ Path)]),
									io:format("~s", [os:cmd("$(MAKE) -C " ++ Path ++ " IS_DEP=1")]),
									code:add_patha(Path ++ "/ebin")
							end
					end
				end || P <- Plugins],
				[case code:load_file(P) of
					{module, P} -> ok;
					_ ->
						case lists:keyfind(plugin_dir, 1, Conf) of
							false -> ok;
							{_, PluginsDir} ->
								ErlFile = "$(call core_native_path,$(DEPS_DIR)/$1/)" ++ PluginsDir ++ "/" ++ atom_to_list(P) ++ ".erl",
								{ok, P, Bin} = compile:file(ErlFile, [binary]),
								{module, P} = code:load_binary(P, ErlFile, Bin)
						end
				end || P <- Plugins],
				[RunPlugin(P, preprocess) || P <- Plugins],
				[RunPlugin(P, pre_compile) || P <- Plugins],
				[RunPlugin(P, compile) || P <- Plugins]
		end
	end(),
	halt()
endef

define dep_autopatch_app.erl
	UpdateModules = fun(App) ->
		case filelib:is_regular(App) of
			false -> ok;
			true ->
				{ok, [{application, '$(1)', L0}]} = file:consult(App),
				Mods = filelib:fold_files("$(call core_native_path,$(DEPS_DIR)/$1/src)", "\\\\.erl$$", true,
					fun (F, Acc) -> [list_to_atom(filename:rootname(filename:basename(F)))|Acc] end, []),
				L = lists:keystore(modules, 1, L0, {modules, Mods}),
				ok = file:write_file(App, io_lib:format("~p.~n", [{application, '$(1)', L}]))
		end
	end,
	UpdateModules("$(call core_native_path,$(DEPS_DIR)/$1/ebin/$1.app)"),
	halt()
endef

define dep_autopatch_appsrc_script.erl
	AppSrc = "$(call core_native_path,$(DEPS_DIR)/$1/src/$1.app.src)",
	AppSrcScript = AppSrc ++ ".script",
	Bindings = erl_eval:new_bindings(),
	{ok, Conf} = file:script(AppSrcScript, Bindings),
	ok = file:write_file(AppSrc, io_lib:format("~p.~n", [Conf])),
	halt()
endef

define dep_autopatch_appsrc.erl
	AppSrcOut = "$(call core_native_path,$(DEPS_DIR)/$1/src/$1.app.src)",
	AppSrcIn = case filelib:is_regular(AppSrcOut) of false -> "$(call core_native_path,$(DEPS_DIR)/$1/ebin/$1.app)"; true -> AppSrcOut end,
	case filelib:is_regular(AppSrcIn) of
		false -> ok;
		true ->
			{ok, [{application, $(1), L0}]} = file:consult(AppSrcIn),
			L1 = lists:keystore(modules, 1, L0, {modules, []}),
			L2 = case lists:keyfind(vsn, 1, L1) of {_, git} -> lists:keyreplace(vsn, 1, L1, {vsn, "git"}); _ -> L1 end,
			L3 = case lists:keyfind(registered, 1, L2) of false -> [{registered, []}|L2]; _ -> L2 end,
			ok = file:write_file(AppSrcOut, io_lib:format("~p.~n", [{application, $(1), L3}])),
			case AppSrcOut of AppSrcIn -> ok; _ -> ok = file:delete(AppSrcIn) end
	end,
	halt()
endef

define dep_fetch_git
	git clone -q -n -- $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)); \
	cd $(DEPS_DIR)/$(call dep_name,$(1)) && git checkout -q $(call dep_commit,$(1));
endef

define dep_fetch_git-submodule
	git submodule update --init -- $(DEPS_DIR)/$1;
endef

define dep_fetch_hg
	hg clone -q -U $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)); \
	cd $(DEPS_DIR)/$(call dep_name,$(1)) && hg update -q $(call dep_commit,$(1));
endef

define dep_fetch_svn
	svn checkout -q $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1));
endef

define dep_fetch_cp
	cp -R $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1));
endef

define dep_fetch_hex.erl
	ssl:start(),
	inets:start(),
	{ok, {{_, 200, _}, _, Body}} = httpc:request(get,
		{"https://s3.amazonaws.com/s3.hex.pm/tarballs/$(1)-$(2).tar", []},
		[], [{body_format, binary}]),
	{ok, Files} = erl_tar:extract({binary, Body}, [memory]),
	{_, Source} = lists:keyfind("contents.tar.gz", 1, Files),
	ok = erl_tar:extract({binary, Source}, [{cwd, "$(call core_native_path,$(DEPS_DIR)/$1)"}, compressed]),
	halt()
endef

# Hex only has a package version. No need to look in the Erlang.mk packages.
define dep_fetch_hex
	$(call erlang,$(call dep_fetch_hex.erl,$(1),$(strip $(word 2,$(dep_$(1))))));
endef

define dep_fetch_fail
	echo "Error: Unknown or invalid dependency: $(1)." >&2; \
	exit 78;
endef

# Kept for compatibility purposes with older Erlang.mk configuration.
define dep_fetch_legacy
	$(warning WARNING: '$(1)' dependency configuration uses deprecated format.) \
	git clone -q -n -- $(word 1,$(dep_$(1))) $(DEPS_DIR)/$(1); \
	cd $(DEPS_DIR)/$(1) && git checkout -q $(if $(word 2,$(dep_$(1))),$(word 2,$(dep_$(1))),master);
endef

define dep_fetch
	$(if $(dep_$(1)), \
		$(if $(dep_fetch_$(word 1,$(dep_$(1)))), \
			$(word 1,$(dep_$(1))), \
			$(if $(IS_DEP),legacy,fail)), \
		$(if $(filter $(1),$(PACKAGES)), \
			$(pkg_$(1)_fetch), \
			fail))
endef

define dep_target
$(DEPS_DIR)/$(call dep_name,$1):
	$(eval DEP_NAME := $(call dep_name,$1))
	$(eval DEP_STR := $(if $(filter-out $1,$(DEP_NAME)),$1,"$1 ($(DEP_NAME))"))
	$(verbose) if test -d $(APPS_DIR)/$(DEP_NAME); then \
		echo "Error: Dependency" $(DEP_STR) "conflicts with application found in $(APPS_DIR)/$(DEP_NAME)."; \
		exit 17; \
	fi
	$(verbose) mkdir -p $(DEPS_DIR)
	$(dep_verbose) $(call dep_fetch_$(strip $(call dep_fetch,$(1))),$(1))
	$(verbose) if [ -f $(DEPS_DIR)/$(1)/configure.ac -o -f $(DEPS_DIR)/$(1)/configure.in ] \
			&& [ ! -f $(DEPS_DIR)/$(1)/configure ]; then \
		echo " AUTO  " $(1); \
		cd $(DEPS_DIR)/$(1) && autoreconf -Wall -vif -I m4; \
	fi
	- $(verbose) if [ -f $(DEPS_DIR)/$(DEP_NAME)/configure ]; then \
		echo " CONF  " $(DEP_STR); \
		cd $(DEPS_DIR)/$(DEP_NAME) && ./configure; \
	fi
ifeq ($(filter $(1),$(NO_AUTOPATCH)),)
	$(verbose) if [ "$(1)" = "amqp_client" -a "$(RABBITMQ_CLIENT_PATCH)" ]; then \
		if [ ! -d $(DEPS_DIR)/rabbitmq-codegen ]; then \
			echo " PATCH  Downloading rabbitmq-codegen"; \
			git clone https://github.com/rabbitmq/rabbitmq-codegen.git $(DEPS_DIR)/rabbitmq-codegen; \
		fi; \
		if [ ! -d $(DEPS_DIR)/rabbitmq-server ]; then \
			echo " PATCH  Downloading rabbitmq-server"; \
			git clone https://github.com/rabbitmq/rabbitmq-server.git $(DEPS_DIR)/rabbitmq-server; \
		fi; \
		ln -s $(DEPS_DIR)/amqp_client/deps/rabbit_common-0.0.0 $(DEPS_DIR)/rabbit_common; \
	elif [ "$(1)" = "rabbit" -a "$(RABBITMQ_SERVER_PATCH)" ]; then \
		if [ ! -d $(DEPS_DIR)/rabbitmq-codegen ]; then \
			echo " PATCH  Downloading rabbitmq-codegen"; \
			git clone https://github.com/rabbitmq/rabbitmq-codegen.git $(DEPS_DIR)/rabbitmq-codegen; \
		fi \
	else \
		$$(call dep_autopatch,$(DEP_NAME)) \
	fi
endif
endef

$(foreach dep,$(BUILD_DEPS) $(DEPS),$(eval $(call dep_target,$(dep))))

ifndef IS_APP
clean:: clean-apps

clean-apps:
	$(verbose) for dep in $(ALL_APPS_DIRS) ; do \
		$(MAKE) -C $$dep clean IS_APP=1 || exit $$?; \
	done

distclean:: distclean-apps

distclean-apps:
	$(verbose) for dep in $(ALL_APPS_DIRS) ; do \
		$(MAKE) -C $$dep distclean IS_APP=1 || exit $$?; \
	done
endif

ifndef SKIP_DEPS
distclean:: distclean-deps

distclean-deps:
	$(gen_verbose) rm -rf $(DEPS_DIR)
endif

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
