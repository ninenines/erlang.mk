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
	if [ -f $(DEPS_DIR)/$(1)/rebar.config.script ]; then \
		$(call dep_autopatch_rebar_script,$(1)); \
		$(call dep_autopatch_rebar,$(1)); \
	elif [ -f $(DEPS_DIR)/$(1)/rebar.config ]; then \
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

define dep_autopatch_rebar_script
	mv $(DEPS_DIR)/$(1)/rebar.config.script $(DEPS_DIR)/$(1)/rebar.config.script.orig; \
	sed -r 's/rebar_utils:is_arch\((.*)\)/\1 =:= "$(PLATFORM)"/g' $(DEPS_DIR)/$(1)/rebar.config.script.orig \
		> $(DEPS_DIR)/$(1)/rebar.config.script
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
	Conf = case filelib:is_file("$(DEPS_DIR)/$(1)/rebar.config.script") of
		false -> Conf1;
		true ->
			Bindings0 = erl_eval:new_bindings(),
			Bindings1 = erl_eval:add_binding('CONFIG', Conf1, Bindings0),
			Bindings = erl_eval:add_binding('SCRIPT', "$(DEPS_DIR)/$(1)/rebar.config.script", Bindings1),
			{ok, Conf2} = file:script("$(DEPS_DIR)/$(1)/rebar.config.script", Bindings),
			Conf2
	end,
	Write = fun (Text) ->
		file:write_file("$(DEPS_DIR)/$(1)/Makefile", Text, [append])
	end,
	Escape = fun (Text) ->
		re:replace(Text, "\\\\$$$$", "\$$$$$$$$", [global, {return, list}])
	end,
	Write("IGNORE_DEPS = edown eper eunit_formatters meck "
		"rebar_lock_deps_plugin rebar_vsn_plugin reltool_util\n\n"),
	fun() ->
		Write("ERLC_OPTS = +debug_info\n"),
		case lists:keyfind(erl_opts, 1, Conf) of
			false -> ok;
			{_, ErlOpts} ->
				lists:foreach(fun
					({d, D}) ->
						Write("ERLC_OPTS += -D" ++ atom_to_list(D) ++ "=1\n");
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
			{eof, _} ->
				file:close(Fd),
				[];
			_ ->
				F(F, Fd)
		end
	end,
	fun() ->
		ErlFiles = filelib:wildcard("$(DEPS_DIR)/$(1)/src/*.erl"),
		First = lists:usort(lists:flatten([begin
			{ok, Fd} = file:open(F, [read]),
			FindFirst(FindFirst, Fd)
		end || F <- ErlFiles])),
		Write(["COMPILE_FIRST +=", [[" ", atom_to_list(M)] || M <- First,
			lists:member("$(DEPS_DIR)/$(1)/src/" ++ atom_to_list(M) ++ ".erl", ErlFiles)], "\n"])
	end(),
	PortSpec = fun(Name, {_, Output, Input, [{env, Env}]}) ->
		filelib:ensure_dir("$(DEPS_DIR)/$(1)/" ++ Output),
		file:write_file("$(DEPS_DIR)/$(1)/c_src/Makefile." ++ Name, [
			[["override ", K, " = $$$$\(shell echo ", Escape(V), "\)\n"]
				|| {_, K, V} <- Env],
			"\nall:\n\t$$$$\(CC\) $$$$\(CFLAGS\) $$$$\(LDLIBS\) $$$$\(LDFLAGS\) ",
			"-o $(DEPS_DIR)/$(1)/", Output,
			[[" ../", F] || F <- Input]
		])
	end,
	fun() ->
		case lists:keyfind(port_specs, 1, Conf) of
			{_, [{Output, _}]} ->
				filelib:ensure_dir("$(DEPS_DIR)/$(1)/" ++ Output),
				Write("C_SRC_OUTPUT = " ++ Escape(Output) ++ "\n");
			{_, [First, Second]} ->
				PortSpec("1", First),
				PortSpec("2", Second),
				file:write_file("$(DEPS_DIR)/$(1)/c_src/Makefile",
					"all:\n\t$$$$\(MAKE\) -f Makefile.1\n\t$$$$\(MAKE\) -f Makefile.2\n");
			_ -> ok
		end
	end(),
	fun() ->
		case lists:keyfind(port_env, 1, Conf) of
			{_, Vars} ->
				lists:foldl(fun
					({K, V}, Acc) ->
						case lists:member(K, Acc) of
							true -> Acc;
							false ->
								Write(K ++ " = $$$$\(shell echo " ++ Escape(V) ++ "\)\n"),
								[K|Acc]
						end;
					({Regex, K, V}, Acc) ->
						case lists:member(K, Acc) of
							true -> Acc;
							false ->
								case re:run("$(PLATFORM)", Regex, [{capture, none}]) of
									nomatch -> Acc;
									match ->
										Write(K ++ " = $$$$\(shell echo " ++ Escape(V) ++ "\)\n"),
										[K|Acc]
								end
						end
				end, [], Vars),
				Write("CFLAGS += $$$$\(DRV_CFLAGS\)\n"),
				Write("CXXFLAGS += $$$$\(DRV_CFLAGS\)\n"),
				Write("LDFLAGS += $$$$\(DRV_LDFLAGS\)\n");
			_ -> ok
		end
	end(),
	fun() ->
		case filelib:is_dir("$(DEPS_DIR)/$(1)/c_src") of
			false -> ok;
			true ->
				Sources = [[" ./c_src/", S] || S <- filelib:wildcard("*.{c,C,cc,cpp}", "$(DEPS_DIR)/$(1)/c_src")],
				Write(io_lib:format("SOURCES := ~s\n", [Sources]))
		end
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
						case re:run("$(PLATFORM)", Regex, [{capture, none}]) of
							match ->
								Command = case Command0 of
									"make -C" ++ _ -> Escape(Command0);
									"gmake -C" ++ _ -> Escape(Command0);
									"make " ++ Command1 -> "make -f Makefile.orig.mk " ++ Escape(Command1);
									"gmake " ++ Command1 -> "gmake -f Makefile.orig.mk " ++ Escape(Command1);
									_ -> Command0
								end,
								Write("\npre-app::\n\t" ++ Command ++ "\n");
							nomatch ->
								ok
						end;
					_ -> ok
				end || H <- Hooks]
		end
	end(),
	Write("\ninclude ../../erlang.mk"),
	fun() ->
		case filelib:is_dir("$(DEPS_DIR)/$(1)/c_src") of
			false -> ok;
			true ->
				Write("\n\nCFLAGS := $$$$\(filter-out -std=c99 -Wmissing-prototypes,$$$$\(CFLAGS\)\)\n")
		end
	end(),
	fun() ->
		case lists:keyfind(plugins, 1, Conf) of
			{_, [Plugin]} when is_atom(Plugin) ->
				ErlFile = "$(DEPS_DIR)/$(1)/plugins/" ++ atom_to_list(Plugin) ++ ".erl",
				try
					{ok, PF} = file:read_file(ErlFile),
					PF2 = re:replace(PF, "rebar_utils:find_files", "find_files", [global, {return, list}]),
					PF3 = PF2 ++ "find_files(Dir, Regex) ->
						filelib:fold_files(Dir, Regex, true, fun(F, Acc) -> [F|Acc] end, []).",
					ok = file:write_file(ErlFile, PF3),
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
