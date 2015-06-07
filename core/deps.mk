# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-deps distclean-pkg pkg-list pkg-search

# Configuration.

IGNORE_DEPS ?=

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR

ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(filter-out $(IGNORE_DEPS),$(DEPS)))

ifeq ($(filter $(DEPS_DIR),$(subst :, ,$(ERL_LIBS))),)
ifeq ($(ERL_LIBS),)
	ERL_LIBS = $(DEPS_DIR)
else
	ERL_LIBS := $(ERL_LIBS):$(DEPS_DIR)
endif
endif
export ERL_LIBS

PKG_FILE2 ?= $(CURDIR)/.erlang.mk.packages.v2
export PKG_FILE2

PKG_FILE_URL ?= https://raw.githubusercontent.com/ninenines/erlang.mk/master/packages.v2.tsv

# Verbosity.

dep_verbose_0 = @echo " DEP   " $(1);
dep_verbose = $(dep_verbose_$(V))

# Core targets.

ifneq ($(SKIP_DEPS),)
deps::
else
deps:: $(ALL_DEPS_DIRS)
	@for dep in $(ALL_DEPS_DIRS) ; do \
		if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ] ; then \
			$(MAKE) -C $$dep IS_DEP=1 || exit $$? ; \
		else \
			echo "ERROR: No Makefile to build dependency $$dep." ; \
			exit 1 ; \
		fi ; \
	done
endif

distclean:: distclean-deps distclean-pkg

# Deps related targets.

# @todo rename GNUmakefile and makefile into Makefile first, if they exist
# While Makefile file could be GNUmakefile or makefile,
# in practice only Makefile is needed so far.
define dep_autopatch
	if [ -f $(DEPS_DIR)/$(1)/Makefile ]; then \
		if [ 0 != `grep -c "include ../\w*\.mk" $(DEPS_DIR)/$(1)/Makefile` ]; then \
			$(call dep_autopatch2,$(1)); \
		elif [ 0 != `grep -ci rebar $(DEPS_DIR)/$(1)/Makefile` ]; then \
			$(call dep_autopatch2,$(1)); \
		elif [ 0 != `find $(DEPS_DIR)/$(1)/ -type f -name \*.mk -not -name erlang.mk | xargs grep -ci rebar` ]; then \
			$(call dep_autopatch2,$(1)); \
		else \
			if [ -f $(DEPS_DIR)/$(1)/erlang.mk ]; then \
				$(call erlang,$(call dep_autopatch_appsrc.erl,$(1))); \
				$(call dep_autopatch_erlang_mk,$(1)); \
			else \
				$(call erlang,$(call dep_autopatch_app.erl,$(1))); \
			fi \
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
	$(call erlang,$(call dep_autopatch_appsrc.erl,$(1))); \
	if [ -f $(DEPS_DIR)/$(1)/rebar.config -o -f $(DEPS_DIR)/$(1)/rebar.config.script ]; then \
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
	rm -f $(DEPS_DIR)/$(1)/erlang.mk; \
	cd $(DEPS_DIR)/$(1)/ && ln -s ../../erlang.mk
endef
else
define dep_autopatch_erlang_mk
	echo -n
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
		git checkout -q 791db716b5a3a7671e0b351f95ddf24b848ee173; \
		make; \
		cd -; \
	fi
endef

define dep_autopatch_rebar
	if [ -f $(DEPS_DIR)/$(1)/Makefile ]; then \
		mv $(DEPS_DIR)/$(1)/Makefile $(DEPS_DIR)/$(1)/Makefile.orig.mk; \
	fi; \
	$(call erlang,$(call dep_autopatch_rebar.erl,$(1)))
endef

define dep_autopatch_rebar.erl
	application:set_env(rebar, log_level, debug),
	Conf1 = case file:consult("$(DEPS_DIR)/$(1)/rebar.config") of
		{ok, Conf0} -> Conf0;
		_ -> []
	end,
	{Conf, OsEnv} = fun() ->
		case filelib:is_file("$(DEPS_DIR)/$(1)/rebar.config.script") of
			false -> {Conf1, []};
			true ->
				Bindings0 = erl_eval:new_bindings(),
				Bindings1 = erl_eval:add_binding('CONFIG', Conf1, Bindings0),
				Bindings = erl_eval:add_binding('SCRIPT', "$(DEPS_DIR)/$(1)/rebar.config.script", Bindings1),
				Before = os:getenv(),
				{ok, Conf2} = file:script("$(DEPS_DIR)/$(1)/rebar.config.script", Bindings),
				{Conf2, lists:foldl(fun(E, Acc) -> lists:delete(E, Acc) end, os:getenv(), Before)}
		end
	end(),
	Write = fun (Text) ->
		file:write_file("$(DEPS_DIR)/$(1)/Makefile", Text, [append])
	end,
	Escape = fun (Text) ->
		re:replace(Text, "\\\\$$$$", "\$$$$$$$$", [global, {return, list}])
	end,
	Write("IGNORE_DEPS = edown eper eunit_formatters meck node_package "
		"rebar_lock_deps_plugin rebar_vsn_plugin reltool_util\n"),
	Write("C_SRC_DIR = /path/do/not/exist\n"),
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
							{N, S} when is_tuple(S) -> {N, S};
							{N, _, S} -> {N, S};
							{N, _, S, _} -> {N, S};
							_ -> false
						end of
					false -> ok;
					{Name, Source} ->
						{Method, Repo, Commit} = case Source of
							{git, R} -> {git, R, master};
							{M, R, {branch, C}} -> {M, R, C};
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
	FindFirst = fun(F, Fd) ->
		case io:parse_erl_form(Fd, undefined) of
			{ok, {attribute, _, compile, {parse_transform, PT}}, _} ->
				[PT, F(F, Fd)];
			{ok, {attribute, _, compile, CompileOpts}, _} when is_list(CompileOpts) ->
				case proplists:get_value(parse_transform, CompileOpts) of
					undefined -> [F(F, Fd)];
					PT -> [PT, F(F, Fd)]
				end;
			{ok, {attribute, _, include, Hrl}, _} ->
				case file:open("$(DEPS_DIR)/$(1)/include/" ++ Hrl, [read]) of
					{ok, HrlFd} -> [F(F, HrlFd), F(F, Fd)];
					_ ->
						case file:open("$(DEPS_DIR)/$(1)/src/" ++ Hrl, [read]) of
							{ok, HrlFd} -> [F(F, HrlFd), F(F, Fd)];
							_ -> [F(F, Fd)]
						end
				end;
			{ok, {attribute, _, include_lib, "$(1)/include/" ++ Hrl}, _} ->
				{ok, HrlFd} = file:open("$(DEPS_DIR)/$(1)/include/" ++ Hrl, [read]),
				[F(F, HrlFd), F(F, Fd)];
			{ok, {attribute, _, include_lib, Hrl}, _} ->
				case file:open("$(DEPS_DIR)/$(1)/include/" ++ Hrl, [read]) of
					{ok, HrlFd} -> [F(F, HrlFd), F(F, Fd)];
					_ -> [F(F, Fd)]
				end;
			{ok, {attribute, _, import, {Imp, _}}, _} ->
				case file:open("$(DEPS_DIR)/$(1)/src/" ++ atom_to_list(Imp) ++ ".erl", [read]) of
					{ok, ImpFd} -> [Imp, F(F, ImpFd), F(F, Fd)];
					_ -> [F(F, Fd)]
				end;
			{eof, _} ->
				file:close(Fd),
				[];
			_ ->
				F(F, Fd)
		end
	end,
	fun() ->
		ErlFiles = filelib:wildcard("$(DEPS_DIR)/$(1)/src/*.erl"),
		First0 = lists:usort(lists:flatten([begin
			{ok, Fd} = file:open(F, [read]),
			FindFirst(FindFirst, Fd)
		end || F <- ErlFiles])),
		First = lists:flatten([begin
			{ok, Fd} = file:open("$(DEPS_DIR)/$(1)/src/" ++ atom_to_list(M) ++ ".erl", [read]),
			FindFirst(FindFirst, Fd)
		end || M <- First0, lists:member("$(DEPS_DIR)/$(1)/src/" ++ atom_to_list(M) ++ ".erl", ErlFiles)]) ++ First0,
		Write(["COMPILE_FIRST +=", [[" ", atom_to_list(M)] || M <- First,
			lists:member("$(DEPS_DIR)/$(1)/src/" ++ atom_to_list(M) ++ ".erl", ErlFiles)], "\n"])
	end(),
	Write("\n\nrebar_dep: preprocess pre-deps deps pre-app app\n"),
	Write("\npreprocess::\n"),
	Write("\npre-deps::\n"),
	Write("\npre-app::\n"),
	PatchHook = fun(Cmd) ->
		case Cmd of
			"make -C" ++ _ -> Escape(Cmd);
			"gmake -C" ++ _ -> Escape(Cmd);
			"make " ++ Cmd1 -> "make -f Makefile.orig.mk " ++ Escape(Cmd1);
			"gmake " ++ Cmd1 -> "gmake -f Makefile.orig.mk " ++ Escape(Cmd1);
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
						Write("\npre-app::\n\tCC=$$$$\(CC) " ++ PatchHook(Cmd) ++ "\n");
					{Regex, compile, Cmd} ->
						case rebar_utils:is_arch(Regex) of
							true -> Write("\npre-app::\n\tCC=$$$$\(CC) " ++ PatchHook(Cmd) ++ "\n");
							false -> ok
						end;
					_ -> ok
				end || H <- Hooks]
		end
	end(),
	ShellToMk = fun(V) ->
		re:replace(re:replace(V, "(\\\\$$$$)(\\\\w*)", "\\\\1(\\\\2)", [global]),
			"-Werror\\\\b", "", [{return, list}, global])
	end,
	PortSpecs = fun() ->
		case lists:keyfind(port_specs, 1, Conf) of
			false ->
				case filelib:is_dir("$(DEPS_DIR)/$(1)/c_src") of
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
		file:write_file("$(DEPS_DIR)/$(1)/c_src/Makefile.erlang.mk", Text, [append])
	end,
	case PortSpecs of
		[] -> ok;
		_ ->
			Write("\npre-app::\n\t$$$$\(MAKE) -f c_src/Makefile.erlang.mk\n"),
			PortSpecWrite(io_lib:format("ERL_CFLAGS = -finline-functions -Wall -fPIC -I ~s/erts-~s/include -I ~s\n",
				[code:root_dir(), erlang:system_info(version), code:lib_dir(erl_interface, include)])),
			PortSpecWrite(io_lib:format("ERL_LDFLAGS = -L ~s -lerl_interface -lei\n",
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
				filelib:ensure_dir("$(DEPS_DIR)/$(1)/" ++ Output),
				Input = [[" ", I] || I <- Input0],
				PortSpecWrite([
					[["\n", K, " = ", ShellToMk(V)] || {K, V} <- lists:reverse(MergeEnv(PortEnv))],
					case $(PLATFORM) of
						darwin -> "\n\nLDFLAGS += -flat_namespace -undefined suppress";
						_ -> ""
					end,
					"\n\nall:: ", Output, "\n\n",
					"%.o: %.c\n\t$$$$\(CC) -c -o $$$$\@ $$$$\< $$$$\(CFLAGS) $$$$\(ERL_CFLAGS) $$$$\(DRV_CFLAGS) $$$$\(EXE_CFLAGS)\n\n",
					"%.o: %.C\n\t$$$$\(CXX) -c -o $$$$\@ $$$$\< $$$$\(CXXFLAGS) $$$$\(ERL_CFLAGS) $$$$\(DRV_CFLAGS) $$$$\(EXE_CFLAGS)\n\n",
					"%.o: %.cc\n\t$$$$\(CXX) -c -o $$$$\@ $$$$\< $$$$\(CXXFLAGS) $$$$\(ERL_CFLAGS) $$$$\(DRV_CFLAGS) $$$$\(EXE_CFLAGS)\n\n",
					"%.o: %.cpp\n\t$$$$\(CXX) -c -o $$$$\@ $$$$\< $$$$\(CXXFLAGS) $$$$\(ERL_CFLAGS) $$$$\(DRV_CFLAGS) $$$$\(EXE_CFLAGS)\n\n",
					[[Output, ": ", K, " = ", ShellToMk(V), "\n"] || {K, V} <- lists:reverse(MergeEnv(FilterEnv(Env)))],
					Output, ": $$$$\(foreach ext,.c .C .cc .cpp,",
						"$$$$\(patsubst %$$$$\(ext),%.o,$$$$\(filter %$$$$\(ext),$$$$\(wildcard", Input, "))))\n",
					"\t$$$$\(CC) -o $$$$\@ $$$$\? $$$$\(LDFLAGS) $$$$\(ERL_LDFLAGS) $$$$\(DRV_LDFLAGS) $$$$\(EXE_LDFLAGS)",
					case filename:extension(Output) of
						[] -> "\n";
						_ -> " -shared\n"
					end])
			end,
			[PortSpec(S) || S <- PortSpecs]
	end,
	Write("\ninclude ../../erlang.mk"),
	RunPlugin = fun(Plugin, Step) ->
		case erlang:function_exported(Plugin, Step, 2) of
			false -> ok;
			true ->
				c:cd("$(DEPS_DIR)/$(1)/"),
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
									Path = "$(DEPS_DIR)/" ++ atom_to_list(P),
									io:format("~s", [os:cmd("$(MAKE) -C $(DEPS_DIR)/$(1) " ++ Path)]),
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
								ErlFile = "$(DEPS_DIR)/$(1)/" ++ PluginsDir ++ "/" ++ atom_to_list(P) ++ ".erl",
								{ok, P, Bin} = compile:file(ErlFile, [binary]),
								{module, P} = code:load_binary(P, ErlFile, Bin)
						end
				end || P <- Plugins],
				[RunPlugin(P, preprocess) || P <- Plugins],
				[RunPlugin(P, pre_compile) || P <- Plugins]
		end
	end(),
	halt()
endef

define dep_autopatch_app.erl
	UpdateModules = fun(App) ->
		case filelib:is_regular(App) of
			false -> ok;
			true ->
				{ok, [{application, $(1), L0}]} = file:consult(App),
				Mods = filelib:fold_files("$(DEPS_DIR)/$(1)/src", "\\\\.erl$$$$", true,
					fun (F, Acc) -> [list_to_atom(filename:rootname(filename:basename(F)))|Acc] end, []),
				L = lists:keystore(modules, 1, L0, {modules, Mods}),
				ok = file:write_file(App, io_lib:format("~p.~n", [{application, $(1), L}]))
		end
	end,
	UpdateModules("$(DEPS_DIR)/$(1)/ebin/$(1).app"),
	halt()
endef

define dep_autopatch_appsrc.erl
	AppSrcOut = "$(DEPS_DIR)/$(1)/src/$(1).app.src",
	AppSrcIn = case filelib:is_regular(AppSrcOut) of false -> "$(DEPS_DIR)/$(1)/ebin/$(1).app"; true -> AppSrcOut end,
	case filelib:is_regular(AppSrcIn) of
		false -> ok;
		true ->
			{ok, [{application, $(1), L0}]} = file:consult(AppSrcIn),
			L1 = lists:keystore(modules, 1, L0, {modules, []}),
			L2 = case lists:keyfind(vsn, 1, L1) of {vsn, git} -> lists:keyreplace(vsn, 1, L1, {vsn, "git"}); _ -> L1 end,
			ok = file:write_file(AppSrcOut, io_lib:format("~p.~n", [{application, $(1), L2}])),
			case AppSrcOut of AppSrcIn -> ok; _ -> ok = file:delete(AppSrcIn) end
	end,
	halt()
endef

define dep_fetch
	if [ "$$$$VS" = "git" ]; then \
		git clone -q -n -- $$$$REPO $(DEPS_DIR)/$(1); \
		cd $(DEPS_DIR)/$(1) && git checkout -q $$$$COMMIT; \
	elif [ "$$$$VS" = "hg" ]; then \
		hg clone -q -U $$$$REPO $(DEPS_DIR)/$(1); \
		cd $(DEPS_DIR)/$(1) && hg update -q $$$$COMMIT; \
	elif [ "$$$$VS" = "svn" ]; then \
		svn checkout -q $$$$REPO $(DEPS_DIR)/$(1); \
	elif [ "$$$$VS" = "cp" ]; then \
		cp -R $$$$REPO $(DEPS_DIR)/$(1); \
	else \
		echo "Unknown or invalid dependency: $(1). Please consult the erlang.mk README for instructions." >&2; \
		exit 78; \
	fi
endef

define dep_target
$(DEPS_DIR)/$(1):
	@mkdir -p $(DEPS_DIR)
ifeq (,$(dep_$(1)))
	@if [ ! -f $(PKG_FILE2) ]; then $(call core_http_get,$(PKG_FILE2),$(PKG_FILE_URL)); fi
	$(dep_verbose) DEPPKG=$$$$(awk 'BEGIN { FS = "\t" }; $$$$1 == "$(1)" { print $$$$2 " " $$$$3 " " $$$$4 }' $(PKG_FILE2);); \
	VS=$$$$(echo $$$$DEPPKG | cut -d " " -f1); \
	REPO=$$$$(echo $$$$DEPPKG | cut -d " " -f2); \
	COMMIT=$$$$(echo $$$$DEPPKG | cut -d " " -f3); \
	$(call dep_fetch,$(1))
else
ifeq (1,$(words $(dep_$(1))))
	$(dep_verbose) VS=git; \
	REPO=$(dep_$(1)); \
	COMMIT=master; \
	$(call dep_fetch,$(1))
else
ifeq (2,$(words $(dep_$(1))))
	$(dep_verbose) VS=git; \
	REPO=$(word 1,$(dep_$(1))); \
	COMMIT=$(word 2,$(dep_$(1))); \
	$(call dep_fetch,$(1))
else
	$(dep_verbose) VS=$(word 1,$(dep_$(1))); \
	REPO=$(word 2,$(dep_$(1))); \
	COMMIT=$(word 3,$(dep_$(1))); \
	$(call dep_fetch,$(1))
endif
endif
endif
	@if [ -f $(DEPS_DIR)/$(1)/configure.ac -o -f $(DEPS_DIR)/$(1)/configure.in ]; then \
		echo " AUTO  " $(1); \
		cd $(DEPS_DIR)/$(1) && autoreconf -Wall -vif -I m4; \
	fi
	-@if [ -f $(DEPS_DIR)/$(1)/configure ]; then \
		echo " CONF  " $(1); \
		cd $(DEPS_DIR)/$(1) && ./configure; \
	fi
ifeq ($(filter $(1),$(NO_AUTOPATCH)),)
	@$(call dep_autopatch,$(1))
endif
endef

$(foreach dep,$(DEPS),$(eval $(call dep_target,$(dep))))

distclean-deps:
	$(gen_verbose) rm -rf $(DEPS_DIR)

# Packages related targets.

$(PKG_FILE2):
	@$(call core_http_get,$(PKG_FILE2),$(PKG_FILE_URL))

pkg-list: $(PKG_FILE2)
	@cat $(PKG_FILE2) | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$3 "\n" \
		"Website:\t" $$5 "\n" \
		"Description:\t" $$6 "\n" }'

ifdef q
pkg-search: $(PKG_FILE2)
	@cat $(PKG_FILE2) | grep -i ${q} | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$3 "\n" \
		"Website:\t" $$5 "\n" \
		"Description:\t" $$6 "\n" }'
else
pkg-search:
	$(error Usage: $(MAKE) pkg-search q=STRING)
endif

ifeq ($(PKG_FILE2),$(CURDIR)/.erlang.mk.packages.v2)
distclean-pkg:
	$(gen_verbose) rm -f $(PKG_FILE2)
endif

help::
	@printf "%s\n" "" \
		"Package-related targets:" \
		"  pkg-list              List all known packages" \
		"  pkg-search q=STRING   Search for STRING in the package index"
