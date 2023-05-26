# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-deps clean-tmp-deps.log

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

REBAR3_GIT ?= https://github.com/erlang/rebar3
REBAR3_COMMIT ?= 3f563feaf1091a1980241adefa83a32dd2eebf7c # 3.20.0

CACHE_DEPS ?= 0

CACHE_DIR ?= $(if $(XDG_CACHE_HOME),$(XDG_CACHE_HOME),$(HOME)/.cache)/erlang.mk
export CACHE_DIR

# External "early" plugins (see core/plugins.mk for regular plugins).
# They both use the core_dep_plugin macro.

define core_dep_plugin
ifeq ($(2),$(PROJECT))
-include $$(patsubst $(PROJECT)/%,%,$(1))
else
-include $(DEPS_DIR)/$(1)

$(DEPS_DIR)/$(1): $(DEPS_DIR)/$(2) ;
endif
endef

DEP_EARLY_PLUGINS ?=

$(foreach p,$(DEP_EARLY_PLUGINS),\
	$(eval $(if $(findstring /,$p),\
		$(call core_dep_plugin,$p,$(firstword $(subst /, ,$p))),\
		$(call core_dep_plugin,$p/early-plugins.mk,$p))))

# Query functions.

query_fetch_method = $(if $(dep_$(1)),$(call _qfm_dep,$(word 1,$(dep_$(1)))),$(call _qfm_pkg,$(1)))
_qfm_dep = $(if $(dep_fetch_$(1)),$(1),$(if $(IS_DEP),legacy,fail))
_qfm_pkg = $(if $(pkg_$(1)_fetch),$(pkg_$(1)_fetch),fail)

query_name = $(if $(dep_$(1)),$(1),$(if $(pkg_$(1)_name),$(pkg_$(1)_name),$(1)))

query_repo = $(call _qr,$(1),$(call query_fetch_method,$(1)))
_qr = $(if $(query_repo_$(2)),$(call query_repo_$(2),$(1)),$(call dep_repo,$(1)))

query_repo_default = $(if $(dep_$(1)),$(word 2,$(dep_$(1))),$(pkg_$(1)_repo))
query_repo_git = $(patsubst git://github.com/%,https://github.com/%,$(call query_repo_default,$(1)))
query_repo_git-subfolder = $(call query_repo_git,$(1))
query_repo_git-submodule = -
query_repo_hg = $(call query_repo_default,$(1))
query_repo_svn = $(call query_repo_default,$(1))
query_repo_cp = $(call query_repo_default,$(1))
query_repo_ln = $(call query_repo_default,$(1))
query_repo_hex = https://hex.pm/packages/$(if $(word 3,$(dep_$(1))),$(word 3,$(dep_$(1))),$(1))
query_repo_fail = -
query_repo_legacy = -

query_version = $(call _qv,$(1),$(call query_fetch_method,$(1)))
_qv = $(if $(query_version_$(2)),$(call query_version_$(2),$(1)),$(call dep_commit,$(1)))

query_version_default = $(if $(dep_$(1)_commit),$(dep_$(1)_commit),$(if $(dep_$(1)),$(word 3,$(dep_$(1))),$(pkg_$(1)_commit)))
query_version_git = $(call query_version_default,$(1))
query_version_git-subfolder = $(call query_version_git,$(1))
query_version_git-submodule = -
query_version_hg = $(call query_version_default,$(1))
query_version_svn = -
query_version_cp = -
query_version_ln = -
query_version_hex = $(if $(dep_$(1)_commit),$(dep_$(1)_commit),$(if $(dep_$(1)),$(word 2,$(dep_$(1))),$(pkg_$(1)_commit)))
query_version_fail = -
query_version_legacy = -

query_extra = $(call _qe,$(1),$(call query_fetch_method,$(1)))
_qe = $(if $(query_extra_$(2)),$(call query_extra_$(2),$(1)),-)

query_extra_git = -
query_extra_git-subfolder = $(if $(dep_$(1)),subfolder=$(word 4,$(dep_$(1))),-)
query_extra_git-submodule = -
query_extra_hg = -
query_extra_svn = -
query_extra_cp = -
query_extra_ln = -
query_extra_hex = $(if $(dep_$(1)),package-name=$(word 3,$(dep_$(1))),-)
query_extra_fail = -
query_extra_legacy = -

query_absolute_path = $(addprefix $(DEPS_DIR)/,$(call query_name,$(1)))

# Deprecated legacy query functions.
dep_fetch = $(call query_fetch_method,$(1))
dep_name = $(call query_name,$(1))
dep_repo = $(call query_repo_git,$(1))
dep_commit = $(if $(dep_$(1)_commit),$(dep_$(1)_commit),$(if $(dep_$(1)),$(if $(filter hex,$(word 1,$(dep_$(1)))),$(word 2,$(dep_$(1))),$(word 3,$(dep_$(1)))),$(pkg_$(1)_commit)))

LOCAL_DEPS_DIRS = $(foreach a,$(LOCAL_DEPS),$(if $(wildcard $(APPS_DIR)/$(a)),$(APPS_DIR)/$(a)))
ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(foreach dep,$(filter-out $(IGNORE_DEPS),$(BUILD_DEPS) $(DEPS)),$(call dep_name,$(dep))))

# When we are calling an app directly we don't want to include it here
# otherwise it'll be treated both as an apps and a top-level project.
ALL_APPS_DIRS = $(if $(wildcard $(APPS_DIR)/),$(filter-out $(APPS_DIR),$(shell find $(APPS_DIR) -maxdepth 1 -type d)))
ifdef ROOT_DIR
ifndef IS_APP
ALL_APPS_DIRS := $(filter-out $(APPS_DIR)/$(notdir $(CURDIR)),$(ALL_APPS_DIRS))
endif
endif

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

dep_verbose_0 = @echo " DEP    $1 ($(call dep_commit,$1))";
dep_verbose_2 = set -x;
dep_verbose = $(dep_verbose_$(V))

# Optimization: don't recompile deps unless truly necessary.

ifndef IS_DEP
ifneq ($(MAKELEVEL),0)
$(shell rm -f ebin/dep_built)
endif
endif

# Core targets.

ALL_APPS_DIRS_TO_BUILD = $(if $(LOCAL_DEPS_DIRS)$(IS_APP),$(LOCAL_DEPS_DIRS),$(ALL_APPS_DIRS))

apps:: $(ALL_APPS_DIRS) clean-tmp-deps.log | $(ERLANG_MK_TMP)
# Create ebin directory for all apps to make sure Erlang recognizes them
# as proper OTP applications when using -include_lib. This is a temporary
# fix, a proper fix would be to compile apps/* in the right order.
ifndef IS_APP
ifneq ($(ALL_APPS_DIRS),)
	$(verbose) set -e; for dep in $(ALL_APPS_DIRS) ; do \
		mkdir -p $$dep/ebin; \
	done
endif
endif
# At the toplevel: if LOCAL_DEPS is defined with at least one local app, only
# compile that list of apps. Otherwise, compile everything.
# Within an app: compile all LOCAL_DEPS that are (uncompiled) local apps.
ifneq ($(ALL_APPS_DIRS_TO_BUILD),)
	$(verbose) set -e; for dep in $(ALL_APPS_DIRS_TO_BUILD); do \
		if grep -qs ^$$dep$$ $(ERLANG_MK_TMP)/apps.log; then \
			:; \
		else \
			echo $$dep >> $(ERLANG_MK_TMP)/apps.log; \
			$(MAKE) -C $$dep $(if $(IS_TEST),test-build-app) IS_APP=1; \
		fi \
	done
endif

clean-tmp-deps.log:
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) rm -f $(ERLANG_MK_TMP)/apps.log $(ERLANG_MK_TMP)/deps.log
endif

# Erlang.mk does not rebuild dependencies after they were compiled
# once. If a developer is working on the top-level project and some
# dependencies at the same time, he may want to change this behavior.
# There are two solutions:
#     1. Set `FULL=1` so that all dependencies are visited and
#        recursively recompiled if necessary.
#     2. Set `FORCE_REBUILD=` to the specific list of dependencies that
#        should be recompiled (instead of the whole set).

FORCE_REBUILD ?=

ifeq ($(origin FULL),undefined)
ifneq ($(strip $(force_rebuild_dep)$(FORCE_REBUILD)),)
define force_rebuild_dep
echo "$(FORCE_REBUILD)" | grep -qw "$$(basename "$1")"
endef
endif
endif

ifneq ($(SKIP_DEPS),)
deps::
else
deps:: $(ALL_DEPS_DIRS) apps clean-tmp-deps.log | $(ERLANG_MK_TMP)
ifneq ($(ALL_DEPS_DIRS),)
	$(verbose) set -e; for dep in $(ALL_DEPS_DIRS); do \
		if grep -qs ^$$dep$$ $(ERLANG_MK_TMP)/deps.log; then \
			:; \
		else \
			echo $$dep >> $(ERLANG_MK_TMP)/deps.log; \
			if [ -z "$(strip $(FULL))" ] $(if $(force_rebuild_dep),&& ! ($(call force_rebuild_dep,$$dep)),) && [ ! -L $$dep ] && [ -f $$dep/ebin/dep_built ]; then \
				:; \
			elif [ "$$dep" = "$(DEPS_DIR)/hut" -a "$(HUT_PATCH)" ]; then \
				$(MAKE) -C $$dep app IS_DEP=1; \
				if [ ! -L $$dep ] && [ -d $$dep/ebin ]; then touch $$dep/ebin/dep_built; fi; \
			elif [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ]; then \
				$(MAKE) -C $$dep IS_DEP=1; \
				if [ ! -L $$dep ] && [ -d $$dep/ebin ]; then touch $$dep/ebin/dep_built; fi; \
			else \
				echo "Error: No Makefile to build dependency $$dep." >&2; \
				exit 2; \
			fi \
		fi \
	done
endif
endif

# Deps related targets.

# @todo rename GNUmakefile and makefile into Makefile first, if they exist
# While Makefile file could be GNUmakefile or makefile,
# in practice only Makefile is needed so far.
define dep_autopatch
	if [ -f $(DEPS_DIR)/$(1)/erlang.mk ]; then \
		rm -rf $(DEPS_DIR)/$1/ebin/; \
		$(call erlang,$(call dep_autopatch_appsrc.erl,$(1))); \
		$(call dep_autopatch_erlang_mk,$(1)); \
	elif [ -f $(DEPS_DIR)/$(1)/Makefile ]; then \
		if [ -f $(DEPS_DIR)/$1/rebar.lock ]; then \
			$(call dep_autopatch2,$1); \
		elif [ 0 != `grep -c "include ../\w*\.mk" $(DEPS_DIR)/$(1)/Makefile` ]; then \
			$(call dep_autopatch2,$(1)); \
		elif [ 0 != `grep -ci "^[^#].*rebar" $(DEPS_DIR)/$(1)/Makefile` ]; then \
			$(call dep_autopatch2,$(1)); \
		elif [ -n "`find $(DEPS_DIR)/$(1)/ -type f -name \*.mk -not -name erlang.mk -exec grep -i "^[^#].*rebar" '{}' \;`" ]; then \
			$(call dep_autopatch2,$(1)); \
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
	! test -f $(DEPS_DIR)/$1/ebin/$1.app || \
	mv -n $(DEPS_DIR)/$1/ebin/$1.app $(DEPS_DIR)/$1/src/$1.app.src; \
	rm -f $(DEPS_DIR)/$1/ebin/$1.app; \
	if [ -f $(DEPS_DIR)/$1/src/$1.app.src.script ]; then \
		$(call erlang,$(call dep_autopatch_appsrc_script.erl,$(1))); \
	fi; \
	$(call erlang,$(call dep_autopatch_appsrc.erl,$(1))); \
	if [ -f $(DEPS_DIR)/$(1)/rebar -o -f $(DEPS_DIR)/$(1)/rebar.config -o -f $(DEPS_DIR)/$(1)/rebar.config.script -o -f $(DEPS_DIR)/$1/rebar.lock ]; then \
		$(call dep_autopatch_fetch_rebar); \
		$(call dep_autopatch_rebar,$(1)); \
	else \
		$(call dep_autopatch_gen,$(1)); \
	fi
endef

define dep_autopatch_noop
	printf "noop:\n" > $(DEPS_DIR)/$(1)/Makefile
endef

# Replace "include erlang.mk" with a line that will load the parent Erlang.mk
# if given. Do it for all 3 possible Makefile file names.
ifeq ($(NO_AUTOPATCH_ERLANG_MK),)
define dep_autopatch_erlang_mk
	for f in Makefile makefile GNUmakefile; do \
		if [ -f $(DEPS_DIR)/$1/$$f ]; then \
			sed -i.bak s/'include *erlang.mk'/'include $$(if $$(ERLANG_MK_FILENAME),$$(ERLANG_MK_FILENAME),erlang.mk)'/ $(DEPS_DIR)/$1/$$f; \
		fi \
	done
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

# We use flock/lockf when available to avoid concurrency issues.
define dep_autopatch_fetch_rebar
	if command -v flock >/dev/null; then \
		flock $(ERLANG_MK_TMP)/rebar.lock sh -c "$(call dep_autopatch_fetch_rebar2)"; \
	elif command -v lockf >/dev/null; then \
		lockf $(ERLANG_MK_TMP)/rebar.lock sh -c "$(call dep_autopatch_fetch_rebar2)"; \
	else \
		$(call dep_autopatch_fetch_rebar2); \
	fi
endef

define dep_autopatch_fetch_rebar2
	if [ ! -d $(ERLANG_MK_TMP)/rebar3 ]; then \
		git clone -q -n -- $(REBAR3_GIT) $(ERLANG_MK_TMP)/rebar3; \
		cd $(ERLANG_MK_TMP)/rebar3; \
		git checkout -q $(REBAR3_COMMIT); \
		./bootstrap; \
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
	{module, rebar3} = c:l(rebar3),
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
	ToList = fun
		(V) when is_atom(V) -> atom_to_list(V);
		(V) when is_list(V) -> "'\\"" ++ V ++ "\\"'"
	end,
	fun() ->
		Write("ERLC_OPTS = +debug_info\n"),
		case lists:keyfind(erl_opts, 1, Conf) of
			false -> ok;
			{_, ErlOpts} ->
				lists:foreach(fun
					({d, D}) ->
						Write("ERLC_OPTS += -D" ++ ToList(D) ++ "=1\n");
					({d, DKey, DVal}) ->
						Write("ERLC_OPTS += -D" ++ ToList(DKey) ++ "=" ++ ToList(DVal) ++ "\n");
					({i, I}) ->
						Write(["ERLC_OPTS += -I ", I, "\n"]);
					({platform_define, Regex, D}) ->
						case rebar_utils:is_arch(Regex) of
							true -> Write("ERLC_OPTS += -D" ++ ToList(D) ++ "=1\n");
							false -> ok
						end;
					({parse_transform, PT}) ->
						Write("ERLC_OPTS += +'{parse_transform, " ++ ToList(PT) ++ "}'\n");
					(_) -> ok
				end, ErlOpts)
		end,
		Write("\n")
	end(),
	GetHexVsn2 = fun(N, NP) ->
		case file:consult("$(call core_native_path,$(DEPS_DIR)/$1/rebar.lock)") of
			{ok, Lock} ->
				io:format("~p~n", [Lock]),
				LockPkgs = case lists:keyfind("1.2.0", 1, Lock) of
					{_, LP} ->
						LP;
					_ ->
						case lists:keyfind("1.1.0", 1, Lock) of
							{_, LP} ->
								LP;
							_ ->
								false
						end
				end,
				if
					is_list(LockPkgs) ->
						io:format("~p~n", [LockPkgs]),
						case lists:keyfind(atom_to_binary(N, latin1), 1, LockPkgs) of
							{_, {pkg, _, Vsn}, _} ->
								io:format("~p~n", [Vsn]),
								{N, {hex, NP, binary_to_list(Vsn)}};
							_ ->
								false
						end;
					true ->
						false
				end;
			_ ->
				false
		end
	end,
	GetHexVsn3Common = fun(N, NP, S0) ->
		case GetHexVsn2(N, NP) of
			false ->
				S2 = case S0 of
					" " ++ S1 -> S1;
					_ -> S0
				end,
				S = case length([ok || $$. <- S2]) of
					0 -> S2 ++ ".0.0";
					1 -> S2 ++ ".0";
					_ -> S2
				end,
				{N, {hex, NP, S}};
			NameSource ->
				NameSource
		end
	end,
	GetHexVsn3 = fun
		(N, NP, "~>" ++ S0) ->
			GetHexVsn3Common(N, NP, S0);
		(N, NP, ">=" ++ S0) ->
			GetHexVsn3Common(N, NP, S0);
		(N, NP, S) -> {N, {hex, NP, S}}
	end,
	fun() ->
		File = case lists:keyfind(deps, 1, Conf) of
			false -> [];
			{_, Deps} ->
				[begin case case Dep of
							N when is_atom(N) -> GetHexVsn2(N, N);
							{N, S} when is_atom(N), is_list(S) -> GetHexVsn3(N, N, S);
							{N, {pkg, NP}} when is_atom(N) -> GetHexVsn2(N, NP);
							{N, S, {pkg, NP}} -> GetHexVsn3(N, NP, S);
							{N, S} when is_tuple(S) -> {N, S};
							{N, _, S} -> {N, S};
							{N, _, S, _} -> {N, S};
							_ -> false
						end of
					false -> ok;
					{Name, Source} ->
						{Method, Repo, Commit} = case Source of
							{hex, NPV, V} -> {hex, V, NPV};
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
			{_, Files0} ->
				Files = [begin
					hd(filelib:wildcard("$(call core_native_path,$(DEPS_DIR)/$1/src/**/" ++ filename:rootname(F) ++ ".*rl")))
				end || "src/" ++ F <- Files0],
				Names = [[" ", case lists:reverse(F) of
					"lre." ++ Elif -> lists:reverse(Elif);
					"lrx." ++ Elif -> lists:reverse(Elif);
					"lry." ++ Elif -> lists:reverse(Elif);
					Elif -> lists:reverse(Elif)
				end] || "$(call core_native_path,$(DEPS_DIR)/$1/src/)" ++ F <- Files],
				Write(io_lib:format("COMPILE_FIRST +=~s\n", [Names]))
		end
	end(),
	Write("\n\nrebar_dep: preprocess pre-deps deps pre-app app\n"),
	Write("\npreprocess::\n"),
	Write("\npre-deps::\n"),
	Write("\npre-app::\n"),
	PatchHook = fun(Cmd) ->
		Cmd2 = re:replace(Cmd, "^([g]?make)(.*)( -C.*)", "\\\\1\\\\3\\\\2", [{return, list}]),
		case Cmd2 of
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
					{{pc, compile}, Cmd} ->
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
	ShellToMk = fun(V0) ->
		V1 = re:replace(V0, "[$$][(]", "$$\(shell ", [global]),
		V = re:replace(V1, "([$$])(?![(])(\\\\w*)", "\\\\1(\\\\2)", [global]),
		re:replace(V, "-Werror\\\\b", "", [{return, list}, global])
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
			Write("\npre-app::\n\t@$$\(MAKE) --no-print-directory -f c_src/Makefile.erlang.mk\n"),
			PortSpecWrite(io_lib:format("ERL_CFLAGS ?= -finline-functions -Wall -fPIC -I \\"~s/erts-~s/include\\" -I \\"~s\\"\n",
				[code:root_dir(), erlang:system_info(version), code:lib_dir(erl_interface, include)])),
			PortSpecWrite(io_lib:format("ERL_LDFLAGS ?= -L \\"~s\\" -lei\n",
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
					"\n\nall:: ", Output, "\n\t@:\n\n",
					"%.o: %.c\n\t$$\(CC) -c -o $$\@ $$\< $$\(CFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					"%.o: %.C\n\t$$\(CXX) -c -o $$\@ $$\< $$\(CXXFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					"%.o: %.cc\n\t$$\(CXX) -c -o $$\@ $$\< $$\(CXXFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					"%.o: %.cpp\n\t$$\(CXX) -c -o $$\@ $$\< $$\(CXXFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					[[Output, ": ", K, " += ", ShellToMk(V), "\n"] || {K, V} <- lists:reverse(MergeEnv(FilterEnv(Env)))],
					Output, ": $$\(foreach ext,.c .C .cc .cpp,",
						"$$\(patsubst %$$\(ext),%.o,$$\(filter %$$\(ext),$$\(wildcard", Input, "))))\n",
					"\t$$\(CC) -o $$\@ $$\? $$\(LDFLAGS) $$\(ERL_LDFLAGS) $$\(DRV_LDFLAGS) $$\(LDLIBS) $$\(EXE_LDFLAGS)",
					case {filename:extension(Output), $(PLATFORM)} of
					    {[], _} -> "\n";
					    {".so", darwin} -> "-shared\n";
					    {".dylib", darwin} -> "-shared\n";
					    {_, darwin} -> "\n";
					    _ -> " -shared\n"
					end])
			end,
			[PortSpec(S) || S <- PortSpecs]
	end,
	fun() ->
		case lists:keyfind(plugins, 1, Conf) of
			false -> ok;
			{_, Plugins0} ->
				Plugins = [P || P <- Plugins0, is_tuple(P)],
				case lists:keyfind('lfe-compile', 1, Plugins) of
					false -> ok;
					_ -> Write("\nBUILD_DEPS = lfe lfe.mk\ndep_lfe.mk = git https://github.com/ninenines/lfe.mk master\nDEP_PLUGINS = lfe.mk\n")
				end
		end
	end(),
	Write("\ninclude $$\(if $$\(ERLANG_MK_FILENAME),$$\(ERLANG_MK_FILENAME),erlang.mk)"),
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
			{_, Plugins0} ->
				Plugins = [P || P <- Plugins0, is_atom(P)],
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

define dep_autopatch_appsrc_script.erl
	AppSrc = "$(call core_native_path,$(DEPS_DIR)/$1/src/$1.app.src)",
	AppSrcScript = AppSrc ++ ".script",
	Conf1 = case file:consult(AppSrc) of
		{ok, Conf0} -> Conf0;
		{error, enoent} -> []
	end,
	Bindings0 = erl_eval:new_bindings(),
	Bindings1 = erl_eval:add_binding('CONFIG', Conf1, Bindings0),
	Bindings = erl_eval:add_binding('SCRIPT', AppSrcScript, Bindings1),
	Conf = case file:script(AppSrcScript, Bindings) of
		{ok, [C]} -> C;
		{ok, C} -> C
	end,
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
			L2 = case lists:keyfind(vsn, 1, L1) of
				{_, git} -> lists:keyreplace(vsn, 1, L1, {vsn, lists:droplast(os:cmd("git -C $(DEPS_DIR)/$1 describe --dirty --tags --always"))});
				{_, {cmd, _}} -> lists:keyreplace(vsn, 1, L1, {vsn, "cmd"});
				_ -> L1
			end,
			L3 = case lists:keyfind(registered, 1, L2) of false -> [{registered, []}|L2]; _ -> L2 end,
			ok = file:write_file(AppSrcOut, io_lib:format("~p.~n", [{application, $(1), L3}])),
			case AppSrcOut of AppSrcIn -> ok; _ -> ok = file:delete(AppSrcIn) end
	end,
	halt()
endef

ifeq ($(CACHE_DEPS),1)

define dep_cache_fetch_git
	mkdir -p $(CACHE_DIR)/git; \
	if test -d "$(join $(CACHE_DIR)/git/,$(call dep_name,$1))"; then \
		cd $(join $(CACHE_DIR)/git/,$(call dep_name,$1)); \
		if ! git checkout -q $(call dep_commit,$1); then \
			git remote set-url origin $(call dep_repo,$1) && \
			git pull --all && \
			git cat-file -e $(call dep_commit,$1) 2>/dev/null; \
		fi; \
	else \
		git clone -q -n -- $(call dep_repo,$1) $(join $(CACHE_DIR)/git/,$(call dep_name,$1)); \
	fi; \
	git clone -q --branch $(call dep_commit,$1) --single-branch -- $(join $(CACHE_DIR)/git/,$(call dep_name,$1)) $2
endef

define dep_fetch_git
	$(call dep_cache_fetch_git,$1,$(DEPS_DIR)/$(call dep_name,$1));
endef

define dep_fetch_git-subfolder
	mkdir -p $(ERLANG_MK_TMP)/git-subfolder; \
	$(call dep_cache_fetch_git,$1,$(ERLANG_MK_TMP)/git-subfolder/$(call dep_name,$1)); \
	ln -s $(ERLANG_MK_TMP)/git-subfolder/$(call dep_name,$1)/$(word 4,$(dep_$1)) \
		$(DEPS_DIR)/$(call dep_name,$1);
endef

else

define dep_fetch_git
	git clone -q -n -- $(call dep_repo,$1) $(DEPS_DIR)/$(call dep_name,$1); \
	cd $(DEPS_DIR)/$(call dep_name,$1) && git checkout -q $(call dep_commit,$1);
endef

define dep_fetch_git-subfolder
	mkdir -p $(ERLANG_MK_TMP)/git-subfolder; \
	git clone -q -n -- $(call dep_repo,$1) \
		$(ERLANG_MK_TMP)/git-subfolder/$(call dep_name,$1); \
	cd $(ERLANG_MK_TMP)/git-subfolder/$(call dep_name,$1) \
		&& git checkout -q $(call dep_commit,$1); \
	ln -s $(ERLANG_MK_TMP)/git-subfolder/$(call dep_name,$1)/$(word 4,$(dep_$1)) \
		$(DEPS_DIR)/$(call dep_name,$1);
endef

endif

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

define dep_fetch_ln
	ln -s $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1));
endef

ifeq ($(CACHE_DEPS),1)

# Hex only has a package version. No need to look in the Erlang.mk packages.
define dep_fetch_hex
	mkdir -p $(CACHE_DIR)/hex $(DEPS_DIR)/$1; \
	$(eval hex_tar_name=$(if $(word 3,$(dep_$1)),$(word 3,$(dep_$1)),$1)-$(strip $(word 2,$(dep_$1))).tar) \
	$(if $(wildcard $(CACHE_DIR)/hex/$(hex_tar_name)),,$(call core_http_get,$(CACHE_DIR)/hex/$(hex_tar_name),\
		https://repo.hex.pm/tarballs/$(hex_tar_name);)) \
	tar -xOf $(CACHE_DIR)/hex/$(hex_tar_name) contents.tar.gz | tar -C $(DEPS_DIR)/$1 -xzf -;
endef

else

# Hex only has a package version. No need to look in the Erlang.mk packages.
define dep_fetch_hex
	mkdir -p $(ERLANG_MK_TMP)/hex $(DEPS_DIR)/$1; \
	$(call core_http_get,$(ERLANG_MK_TMP)/hex/$1.tar,\
		https://repo.hex.pm/tarballs/$(if $(word 3,$(dep_$1)),$(word 3,$(dep_$1)),$1)-$(strip $(word 2,$(dep_$1))).tar); \
	tar -xOf $(ERLANG_MK_TMP)/hex/$1.tar contents.tar.gz | tar -C $(DEPS_DIR)/$1 -xzf -;
endef

endif

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

define dep_target
$(DEPS_DIR)/$(call dep_name,$1): | $(ERLANG_MK_TMP)
	$(eval DEP_NAME := $(call dep_name,$1))
	$(eval DEP_STR := $(if $(filter $1,$(DEP_NAME)),$1,"$1 ($(DEP_NAME))"))
	$(verbose) if test -d $(APPS_DIR)/$(DEP_NAME); then \
		echo "Error: Dependency" $(DEP_STR) "conflicts with application found in $(APPS_DIR)/$(DEP_NAME)." >&2; \
		exit 17; \
	fi
	$(verbose) mkdir -p $(DEPS_DIR)
	$(dep_verbose) $(call dep_fetch_$(strip $(call dep_fetch,$(1))),$(1))
	$(verbose) if [ -f $(DEPS_DIR)/$(1)/configure.ac -o -f $(DEPS_DIR)/$(1)/configure.in ] \
			&& [ ! -f $(DEPS_DIR)/$(1)/configure ]; then \
		echo " AUTO  " $(DEP_STR); \
		cd $(DEPS_DIR)/$(1) && autoreconf -Wall -vif -I m4; \
	fi
	- $(verbose) if [ -f $(DEPS_DIR)/$(DEP_NAME)/configure ]; then \
		echo " CONF  " $(DEP_STR); \
		cd $(DEPS_DIR)/$(DEP_NAME) && ./configure; \
	fi
ifeq ($(filter $(1),$(NO_AUTOPATCH)),)
	$(verbose) $$(MAKE) --no-print-directory autopatch-$(DEP_NAME)
endif

.PHONY: autopatch-$(call dep_name,$1)

autopatch-$(call dep_name,$1)::
	$(verbose) if [ "$1" = "elixir" -a "$(ELIXIR_PATCH)" ]; then \
		ln -s lib/elixir/ebin $(DEPS_DIR)/elixir/; \
	else \
		$$(call dep_autopatch,$(call dep_name,$1)) \
	fi
endef

$(foreach dep,$(BUILD_DEPS) $(DEPS),$(eval $(call dep_target,$(dep))))

ifndef IS_APP
clean:: clean-apps

clean-apps:
	$(verbose) set -e; for dep in $(ALL_APPS_DIRS) ; do \
		$(MAKE) -C $$dep clean IS_APP=1; \
	done

distclean:: distclean-apps

distclean-apps:
	$(verbose) set -e; for dep in $(ALL_APPS_DIRS) ; do \
		$(MAKE) -C $$dep distclean IS_APP=1; \
	done
endif

ifndef SKIP_DEPS
distclean:: distclean-deps

distclean-deps:
	$(gen_verbose) rm -rf $(DEPS_DIR)
endif

ifeq ($(CACHE_DEPS),1)
cacheclean:: cacheclean-git cacheclean-hex

cacheclean-git:
	$(gen_verbose) rm -rf $(CACHE_DIR)/git

cacheclean-hex:
	$(gen_verbose) rm -rf $(CACHE_DIR)/hex
endif

# Forward-declare variables used in core/deps-tools.mk. This is required
# in case plugins use them.

ERLANG_MK_RECURSIVE_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-deps-list.log
ERLANG_MK_RECURSIVE_DOC_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-doc-deps-list.log
ERLANG_MK_RECURSIVE_REL_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-rel-deps-list.log
ERLANG_MK_RECURSIVE_TEST_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-test-deps-list.log
ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-shell-deps-list.log

ERLANG_MK_QUERY_DEPS_FILE = $(ERLANG_MK_TMP)/query-deps.log
ERLANG_MK_QUERY_DOC_DEPS_FILE = $(ERLANG_MK_TMP)/query-doc-deps.log
ERLANG_MK_QUERY_REL_DEPS_FILE = $(ERLANG_MK_TMP)/query-rel-deps.log
ERLANG_MK_QUERY_TEST_DEPS_FILE = $(ERLANG_MK_TMP)/query-test-deps.log
ERLANG_MK_QUERY_SHELL_DEPS_FILE = $(ERLANG_MK_TMP)/query-shell-deps.log
