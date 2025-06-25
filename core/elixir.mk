# Copyright (c) 2024, Tyler Hughes <tyler@tylerhughes.dev>
# Copyright (c) 2024, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifeq ($(ELIXIR),system)
# We expect 'elixir' to be on the path.
ELIXIR_BIN ?= $(shell readlink -f `which elixir`)
ELIXIR_LIBS ?= $(abspath $(dir $(ELIXIR_BIN))/../lib)
# Fallback in case 'elixir' is a shim.
ifeq ($(wildcard $(ELIXIR_LIBS)/elixir/),)
ELIXIR_LIBS = $(abspath $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))')/../)
endif
ELIXIR_LIBS := $(ELIXIR_LIBS)
export ELIXIR_LIBS
ERL_LIBS := $(ERL_LIBS):$(ELIXIR_LIBS)
else
ifeq ($(ELIXIR),dep)
ERL_LIBS := $(ERL_LIBS):$(DEPS_DIR)/elixir/lib/
endif
endif

elixirc_verbose_0 = @echo " EXC    $(words $(EX_FILES)) files";
elixirc_verbose_2 = set -x;
elixirc_verbose = $(elixirc_verbose_$(V))

# Unfortunately this currently requires Elixir.
# https://github.com/jelly-beam/verl is a good choice
# for an Erlang implementation, but we already have to
# pull hex_core and Rebar3 so adding yet another pull
# is annoying, especially one that would be necessary
# every time we autopatch Rebar projects. Wait and see.
define hex_version_resolver.erl
	HexVersionResolve = fun(Name, Req) ->
		application:ensure_all_started(ssl),
		application:ensure_all_started(inets),
		Config = $(hex_config.erl),
		case hex_repo:get_package(Config, atom_to_binary(Name)) of
			{ok, {200, _RespHeaders, Package}} ->
				#{releases := List} = Package,
				{value, #{version := Version}} = lists:search(fun(#{version := Vsn}) ->
					M = list_to_atom("Elixir.Version"),
					F = list_to_atom("match?"),
					M:F(Vsn, Req)
				end, List),
				{ok, Version};
			{ok, {Status, _, Errors}} ->
				{error, Status, Errors}
		end
	end,
	HexVersionResolveAndPrint = fun(Name, Req) ->
		case HexVersionResolve(Name, Req) of
			{ok, Version} ->
				io:format("~s", [Version]),
				halt(0);
			{error, Status, Errors} ->
				io:format("Error ~b: ~0p~n", [Status, Errors]),
				halt(77)
		end
	end
endef

define dep_autopatch_mix.erl
	$(call hex_version_resolver.erl),
	{ok, _} = application:ensure_all_started(elixir),
	{ok, _} = application:ensure_all_started(mix),
	MixFile = <<"$(call core_native_path,$(DEPS_DIR)/$1/mix.exs)">>,
	{Mod, Bin} =
		case elixir_compiler:file(MixFile, fun(_File, _LexerPid) -> ok end) of
			[{T = {_, _}, _CheckerPid}] -> T;
			[T = {_, _}] -> T
		end,
	{module, Mod} = code:load_binary(Mod, binary_to_list(MixFile), Bin),
	Project = Mod:project(),
	Application = try Mod:application() catch error:undef -> [] end,
	StartMod = case lists:keyfind(mod, 1, Application) of
		{mod, {StartMod0, _StartArgs}} ->
			atom_to_list(StartMod0);
		_ ->
			""
	end,
	Write = fun (Text) ->
		file:write_file("$(call core_native_path,$(DEPS_DIR)/$1/Makefile)", Text, [append])
	end,
	Write([
		"PROJECT = ", atom_to_list(proplists:get_value(app, Project)), "\n"
		"PROJECT_DESCRIPTION = ", proplists:get_value(description, Project, ""), "\n"
		"PROJECT_VERSION = ", proplists:get_value(version, Project, ""), "\n"
		"PROJECT_MOD = ", StartMod, "\n"
		"define PROJECT_ENV\n",
		io_lib:format("~p", [proplists:get_value(env, Application, [])]), "\n"
		"endef\n\n"]),
	ExtraApps = lists:usort([eex, elixir, logger, mix] ++ proplists:get_value(extra_applications, Application, [])),
	Write(["LOCAL_DEPS += ", lists:join(" ", [atom_to_list(App) || App <- ExtraApps]), "\n\n"]),
	Deps = proplists:get_value(deps, Project, []) -- [elixir_make],
	IsRequiredProdDep = fun(Opts) ->
		(proplists:get_value(optional, Opts) =/= true)
		andalso
		case proplists:get_value(only, Opts, prod) of
			prod -> true;
			L when is_list(L) -> lists:member(prod, L);
			_ -> false
		end
	end,
	lists:foreach(fun
		({Name, Req}) when is_binary(Req) ->
			{ok, Vsn} = HexVersionResolve(Name, Req),
			Write(["DEPS += ", atom_to_list(Name), "\n"]),
			Write(["dep_", atom_to_list(Name), " = hex ", Vsn, " ", atom_to_list(Name), "\n"]);
		({Name, Opts}) when is_list(Opts) ->
			Path = proplists:get_value(path, Opts),
			case IsRequiredProdDep(Opts) of
				true when Path =/= undefined ->
					Write(["DEPS += ", atom_to_list(Name), "\n"]),
					Write(["dep_", atom_to_list(Name), " = ln ", Path, "\n"]);
				true when Path =:= undefined ->
					Write(["DEPS += ", atom_to_list(Name), "\n"]),
					io:format(standard_error, "Warning: No version given for ~p.", [Name]);
				false ->
					ok
			end;
		({Name, Req, Opts}) ->
			case IsRequiredProdDep(Opts) of
				true ->
					{ok, Vsn} = HexVersionResolve(Name, Req),
					Write(["DEPS += ", atom_to_list(Name), "\n"]),
					Write(["dep_", atom_to_list(Name), " = hex ", Vsn, " ", atom_to_list(Name), "\n"]);
				false ->
					ok
			end;
		(_) ->
			ok
	end, Deps),
	case lists:member(elixir_make, proplists:get_value(compilers, Project, [])) of
		false -> 
			ok;
		true ->
			Write("# https://hexdocs.pm/elixir_make/Mix.Tasks.Compile.ElixirMake.html\n"),
			MakeVal = fun(Key, Proplist, DefaultVal, DefaultReplacement) ->
				case proplists:get_value(Key, Proplist, DefaultVal) of
					DefaultVal -> DefaultReplacement;
					Value -> Value
				end
			end,
			MakeMakefile = binary_to_list(MakeVal(make_makefile, Project, default, <<"Makefile">>)),
			MakeExe = MakeVal(make_executable, Project, default, "$$\(MAKE)"),
			MakeCwd = MakeVal(make_cwd, Project, undefined, <<".">>),
			MakeTargets = MakeVal(make_targets, Project, [], []),
			MakeArgs = MakeVal(make_args, Project, undefined, []),
			case file:rename("$(DEPS_DIR)/$1/" ++ MakeMakefile, "$(DEPS_DIR)/$1/elixir_make.mk") of
				ok -> ok;
				Err = {error, _} ->
					io:format(standard_error, "Failed to copy Makefile with error ~p~n", [Err]),
					halt(90)
			end,
			Write(["app::\n"
				"\t", MakeExe, " -C ", MakeCwd, " -f $(DEPS_DIR)/$1/elixir_make.mk",
				lists:join(" ", MakeTargets),
				lists:join(" ", MakeArgs),
				"\n\n"]),
			case MakeVal(make_clean, Project, nil, undefined) of
				undefined ->
					ok;
				Clean ->
					Write(["clean::\n\t", Clean, "\n\n"])
			end
	end,
	Write("ERLC_OPTS = +debug_info\n\n"),
	Write("include $$\(if $$\(ERLANG_MK_FILENAME),$$\(ERLANG_MK_FILENAME),erlang.mk)"),
	halt()
endef

define dep_autopatch_mix
	sed 's|\(defmodule.*do\)|\1\n  try do\n    Code.compiler_options(on_undefined_variable: :warn)\n    rescue _ -> :ok\n  end\n|g' -i $(DEPS_DIR)/$(1)/mix.exs; \
	$(MAKE) $(DEPS_DIR)/hex_core/ebin/dep_built; \
	MIX_ENV="$(if $(MIX_ENV),$(strip $(MIX_ENV)),prod)" \
		$(call erlang,$(call dep_autopatch_mix.erl,$1))
endef

# We change the group leader so the Elixir io:format output
# isn't captured as we need to either print the modules on
# success, or print _ERROR_ on failure.
define compile_ex.erl
	{ok, _} = application:ensure_all_started(elixir),
	{ok, _} = application:ensure_all_started(mix),
	$(foreach dep,$(LOCAL_DEPS),_ = application:load($(dep)),)
	ModCode = list_to_atom("Elixir.Code"),
	ModCode:put_compiler_option(ignore_module_conflict, true),
	ModComp = list_to_atom("Elixir.Kernel.ParallelCompiler"),
	ModMixProject = list_to_atom("Elixir.Mix.Project"),
	erlang:group_leader(whereis(standard_error), self()),
	ModMixProject:in_project($(PROJECT), ".", [], fun(_MixFile) ->
		case ModComp:compile_to_path([$(call comma_list,$(patsubst %,<<"%">>,$1))], <<"ebin/">>) of
			{ok, Modules, _} ->
				lists:foreach(fun(E) -> io:format(user, "~p ", [E]) end, Modules),
				halt(0);
			{error, _ErroredModules, _WarnedModules} ->
				io:format(user, "_ERROR_", []),
				halt(1)
		end
	end)
endef
