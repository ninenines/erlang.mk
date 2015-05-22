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
			$(call dep_autopatch_erlang_mk,$(1)); \
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
	if [ -f $(DEPS_DIR)/$(1)/rebar.config -o -f $(DEPS_DIR)/$(1)/rebar.config.script ]; then \
		$(call dep_autopatch_rebar_utils); \
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
	cd $(DEPS_DIR)/$(1)/ && ln -s ../../erlang.mk; \
	$(call erlang,$(call dep_autopatch_appsrc.erl,$(1)))
endef
else
define dep_autopatch_erlang_mk
	$(call erlang,$(call dep_autopatch_appsrc.erl,$(1)))
endef
endif

define dep_autopatch_gen
	printf "%s\n" \
		"ERLC_OPTS = +debug_info" \
		"include ../../erlang.mk" > $(DEPS_DIR)/$(1)/Makefile; \
	$(call erlang,$(call dep_autopatch_appsrc.erl,$(1)))
endef

define dep_autopatch_rebar_utils
	mkdir -p $(ERLANG_MK_TMP)/ebin; \
	if [ ! -f $(ERLANG_MK_TMP)/rebar.hrl ]; then \
		$(call core_http_get,$(ERLANG_MK_TMP)/rebar.hrl,https://raw.githubusercontent.com/rebar/rebar/791db716b5a3a7671e0b351f95ddf24b848ee173/include/rebar.hrl); \
	fi; \
	if [ ! -f $(ERLANG_MK_TMP)/rebar_utils.erl ]; then \
		$(call core_http_get,$(ERLANG_MK_TMP)/rebar_utils.erl,https://raw.githubusercontent.com/rebar/rebar/791db716b5a3a7671e0b351f95ddf24b848ee173/src/rebar_utils.erl); \
	fi; \
	if [ ! -f $(ERLANG_MK_TMP)/ebin/rebar_utils.beam ]; then \
		erlc -o $(ERLANG_MK_TMP)/ebin $(ERLANG_MK_TMP)/rebar_utils.erl; \
	fi
endef

define dep_autopatch_rebar
	if [ -f $(DEPS_DIR)/$(1)/Makefile ]; then \
		mv $(DEPS_DIR)/$(1)/Makefile $(DEPS_DIR)/$(1)/Makefile.orig.mk; \
	fi; \
	$(call erlang,$(call dep_autopatch_rebar.erl,$(1))); \
	$(call erlang,$(call dep_autopatch_appsrc.erl,$(1)))
endef

define dep_autopatch_rebar.erl
	Conf1 = case file:consult("$(DEPS_DIR)/$(1)/rebar.config") of
		{ok, Conf0} -> Conf0;
		_ -> []
	end,
	{Conf, OsEnv} = case filelib:is_file("$(DEPS_DIR)/$(1)/rebar.config.script") of
		false -> {Conf1, []};
		true ->
			Bindings0 = erl_eval:new_bindings(),
			Bindings1 = erl_eval:add_binding('CONFIG', Conf1, Bindings0),
			Bindings = erl_eval:add_binding('SCRIPT', "$(DEPS_DIR)/$(1)/rebar.config.script", Bindings1),
			Before = os:getenv(),
			{ok, Conf2} = file:script("$(DEPS_DIR)/$(1)/rebar.config.script", Bindings),
			{Conf2, lists:foldl(fun(E, Acc) -> lists:delete(E, Acc) end, os:getenv(), Before)}
	end,
	Write = fun (Text) ->
		file:write_file("$(DEPS_DIR)/$(1)/Makefile", Text, [append])
	end,
	Escape = fun (Text) ->
		re:replace(Text, "\\\\$$$$", "\$$$$$$$$", [global, {return, list}])
	end,
	Write("IGNORE_DEPS = edown eper eunit_formatters meck node_package "
		"rebar_lock_deps_plugin rebar_vsn_plugin reltool_util\n\n"),
	Write("C_SRC_DIR = /path/do/not/exist\n\n"),
	Write("DRV_CFLAGS = -fPIC\nexport DRV_CFLAGS\n\n"),
	fun() ->
		Write("ERLC_OPTS = +debug_info\n"),
		case lists:keyfind(erl_opts, 1, Conf) of
			false -> ok;
			{_, ErlOpts} ->
				lists:foreach(fun
					({d, D}) ->
						Write("ERLC_OPTS += -D" ++ atom_to_list(D) ++ "=1\n");
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
				[begin
					Name = element(1, Dep),
					{Method, Repo, Commit} = case element(3, Dep) of
						{git, R} -> {git, R, master};
						{M, R, {branch, C}} -> {M, R, C};
						{M, R, {tag, C}} -> {M, R, C};
						{M, R, C} -> {M, R, C}
					end,
					Write(io_lib:format("DEPS += ~s\ndep_~s = ~s ~s ~s~n", [Name, Name, Method, Repo, Commit]))
				end || Dep <- Deps, tuple_size(Dep) > 2]
		end
	end(),
	fun() ->
		First = case lists:keyfind(erl_first_files, 1, Conf) of false -> []; {_, Files} ->
			Names = [[" ", begin "lre." ++ Elif = lists:reverse(F), lists:reverse(Elif) end]
				 || "src/" ++ F <- Files],
			Write(io_lib:format("COMPILE_FIRST +=~s\n", [Names]))
		end
	end(),
	FindFirst = fun(F, Fd) ->
		case io:parse_erl_form(Fd, undefined) of
			{ok, {attribute, _,compile, {parse_transform, PT}}, _} ->
				[PT, F(F, Fd)];
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
	Write("\n\nrebar_dep: pre-deps deps pre-app app\n"),
	Write("\npre-deps::\n"),
	Write("\npre-app::\n"),
	fun() ->
		case lists:keyfind(pre_hooks, 1, Conf) of
			false -> ok;
			{_, Hooks} ->
				[case H of
					{'get-deps', Command} ->
						Write("\npre-deps::\n\t" ++ Escape(Command) ++ "\n");
					{compile, Command} ->
						Write("\npre-app::\n\t" ++ Escape(Command) ++ "\n");
					{Regex, compile, Command0} ->
						case rebar_utils:is_arch(Regex) of
							true ->
								Command = case Command0 of
									"make -C" ++ _ -> Escape(Command0);
									"gmake -C" ++ _ -> Escape(Command0);
									"make " ++ Command1 -> "make -f Makefile.orig.mk " ++ Escape(Command1);
									"gmake " ++ Command1 -> "gmake -f Makefile.orig.mk " ++ Escape(Command1);
									_ -> Command0
								end,
								Write("\npre-app::\n\t" ++ Command ++ "\n");
							false ->
								ok
						end;
					_ -> ok
				end || H <- Hooks]
		end
	end(),
	ShellToMk = fun(V) ->
		re:replace(V, "(\\\\$$$$)(\\\\w*)", "\\\\1(\\\\2)", [{return, list}, global])
	end,
	PortSpecs = fun() ->
		case lists:keyfind(port_specs, 1, Conf) of
			false ->
				case filelib:wildcard("$(DEPS_DIR)/$(1)/c_src/*.c") of
					[] -> [];
					Src -> [{"priv/$(1)_drv.so", ["c_src/*.c"], []}]
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
			PortSpecWrite(["\nERLANG_ARCH = ", rebar_utils:wordsize(), "\n"]),
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
					[["\n", K, " = ", ShellToMk(V)] || {K, V} <- lists:reverse(MergeEnv(PortEnv ++ FilterEnv(Env)))],
					"\n\nall:: ", Output, "\n\n",
					"%.o: %.c\n\t$$$$\(CC) -c -o $$$$\@ $$$$\< $$$$\(CFLAGS) $$$$\(ERL_CFLAGS) $$$$\(DRV_CFLAGS) $$$$\(EXE_CFLAGS)\n\n",
					"%.o: %.C\n\t$$$$\(CXX) -c -o $$$$\@ $$$$\< $$$$\(CXXFLAGS) $$$$\(ERL_CFLAGS) $$$$\(DRV_CFLAGS) $$$$\(EXE_CFLAGS)\n\n",
					"%.o: %.cc\n\t$$$$\(CXX) -c -o $$$$\@ $$$$\< $$$$\(CXXFLAGS) $$$$\(ERL_CFLAGS) $$$$\(DRV_CFLAGS) $$$$\(EXE_CFLAGS)\n\n",
					"%.o: %.cpp\n\t$$$$\(CXX) -c -o $$$$\@ $$$$\< $$$$\(CXXFLAGS) $$$$\(ERL_CFLAGS) $$$$\(DRV_CFLAGS) $$$$\(EXE_CFLAGS)\n\n",
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
	fun() ->
		case lists:keyfind(plugins, 1, Conf) of
			{_, [Plugin]} when is_atom(Plugin) ->
				ErlFile = "$(DEPS_DIR)/$(1)/plugins/" ++ atom_to_list(Plugin) ++ ".erl",
				try
					{ok, Mod, Bin} = compile:file(ErlFile, [binary]),
					{module, Mod} = code:load_binary(Mod, ErlFile, Bin),
					c:cd("$(DEPS_DIR)/$(1)/"),
					ok = Mod:pre_compile(Conf, undefined)
				catch _:_ ->
					ok
				end;
			_ -> ok
		end
	end(),
	halt()
endef

define dep_autopatch_appsrc.erl
	AppSrcOut = "$(DEPS_DIR)/$(1)/src/$(1).app.src",
	AppSrcIn = case filelib:is_regular(AppSrcOut) of false -> "$(DEPS_DIR)/$(1)/ebin/$(1).app"; true -> AppSrcOut end,
	case filelib:is_regular(AppSrcIn) of
		false -> ok;
		true ->
			fun() ->
				{ok, [{application, $(1), L}]} = file:consult(AppSrcIn),
				L2 = case lists:keyfind(modules, 1, L) of {_, _} -> L; false -> [{modules, []}|L] end,
				L3 = case lists:keyfind(vsn, 1, L2) of {vsn, git} -> lists:keyreplace(vsn, 1, L2, {vsn, "git"}); _ -> L2 end,
				ok = file:write_file(AppSrcOut, io_lib:format("~p.~n", [{application, $(1), L3}]))
			end(),
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
	@if [ -f $(DEPS_DIR)/$(1)/configure.ac ]; then \
		echo " AUTO  " $(1); \
		cd $(DEPS_DIR)/$(1) && autoreconf -vif; \
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
