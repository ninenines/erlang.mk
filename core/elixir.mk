# Copyright (c) 2024, Tyler Hughes <artman41@gmail.com>
# Copyright (c) 2024, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ELIXIR ?= $(if $(filter elixir,$(BUILD_DEPS) $(DEPS)),dep,system)
export ELIXIR

ifeq ($(ELIXIR),system)
# We expect 'elixir' to be on the path.
# @todo Only if there are EX_FILES
ELIXIR_LIBS ?= $(dir $(shell readlink -f `which elixir`))/../lib
ELIXIR_LIBS := $(ELIXIR_LIBS)
export ELIXIR_LIBS
ERL_LIBS := $(ERL_LIBS):$(ELIXIR_LIBS)
else
ERL_LIBS := $(ERL_LIBS):$(DEPS_DIR)/elixir/lib/
endif

elixirc_verbose_0 = @echo " EXC    $(words $(EX_FILES)) files";
elixirc_verbose_2 = set -x;
elixirc_verbose = $(elixirc_verbose_$(V))

define dep_autopatch_mix.erl
	$(call hex_version_resolver.erl),
	{ok, _} = application:ensure_all_started(elixir),
	{ok, _} = application:ensure_all_started(mix),
	MixFile = <<"$(call core_native_path,$(DEPS_DIR)/$1/mix.exs)">>,
	{Mod, Bin} =
		case elixir_compiler:file(MixFile, fun(_File, _LexerPid) -> ok end) of
			[{T = {_, _}, _CheckerPid}] -> T
			[T = {_, _}] -> T;
		end,
	{module, Mod} = code:load_binary(Mod, binary_to_list(MixFile), Bin),
	Project = Mod:project(),
	Application = try Mod:application() catch error:undef -> [] end,
	StartMod = case lists:keyfind(mod, 1, Application) of
		{mod, {StartMod0, _StartArgs}} ->
			StartMod0;
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

define compile_ex.erl
	{ok, _} = application:ensure_all_started(elixir),
	{ok, _} = application:ensure_all_started(mix),
	ModCode = list_to_atom("Elixir.Code"),
	ModCode:put_compiler_option(ignore_module_conflict, true),
	ModComp = list_to_atom("Elixir.Kernel.ParallelCompiler"),
	case ModComp:compile_to_path([$(call comma_list,$(patsubst %,<<"%">>,$(EX_FILES)))], <<"ebin/">>) of
		{ok, Modules, _} ->
			halt(0);
		{error, [], _WarnedModules} ->
			halt(0);
		{error, _ErroredModules, _WarnedModules} ->
			halt(1)
	end
endef
