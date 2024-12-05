ELIXIR_USE_SYSTEM ?= 1

ifeq ($(ELIXIR_USE_SYSTEM),1)
ELIXIRC := $(shell which elixirc)
ifneq ($(ELIXIRC),)
ELIXIR_PATH := $(shell $(dir $(ELIXIRC))/elixir -e 'IO.puts(:code.lib_dir(:elixir))')/../../
endif
endif

ifeq ($(ELIXIRC),)
ELIXIR_USE_SYSTEM = 0
ELIXIRC := $(DEPS_DIR)/$(call dep_name,elixir)/bin/elixirc
ELIXIR_PATH = $(DEPS_DIR)/$(call dep_name,elixir)
endif

ELIXIRC := $(abspath $(ELIXIRC))
ELIXIR_PATH := $(abspath $(ELIXIR_PATH))

ELIXIRC_OPTS += $(ERLC_OPTS)

elixirc_verbose_0 = @echo " ELIXIRC  " $(filter-out $(patsubst %,%.ex,$(ERLC_EXCLUDE)),\
	$(filter %.ex %.core,$(?F)));
elixirc_verbose_2 = set -x;
elixirc_verbose = $(elixirc_verbose_$(V))

ALL_LIB_FILES := $(sort $(call core_find,lib/,*))

EX_FILES := $(filter-out lib/mix/%,$(filter %.ex,$(ALL_SRC_FILES) $(ALL_LIB_FILES)))
ELIXIR_BUILTINS = $(addprefix $(ELIXIR_PATH)/lib/,eex elixir logger mix)
USES_ELIXIR = $(if $(EX_FILES),1,)

ifneq ($(USES_ELIXIR),)
ERL_LIBS := $(ERL_LIBS):$(ELIXIR_PATH)/lib/
export ERL_LIBS

app:: $(if $(wildcard ebin/test),clean) deps

define validate_app_file
	case file:consult("ebin/$(PROJECT).app") of
		{ok, _} -> halt();
		_ -> halt(1)
	end
endef
endif

define elixir_get_deps.erl
(fun(Deps) ->
	$(call hex_version_resolver.erl),
	(fun
		F([], DEPS_Acc0, DEP_Acc0) ->
			[DEPS_Acc0, "\n", DEP_Acc0];
		F([H|T], DEPS_Acc0, DEP_Acc0) ->
			{DEPS_Acc1, DEP_Acc1} =
				case H of
					{Name, Req} when is_binary(Req) ->
						{
							[DEPS_Acc0, io_lib:format("DEPS += ~p~n", [Name])],
							[DEP_Acc0, io_lib:format("dep_~p = hex ~s ~p~n", [Name, HexVersionResolve(Name, Req), Name])]
						};
					{Name, Opts} when is_list(Opts) ->
						Path = proplists:get_value(path, Opts),
						IsRequired = proplists:get_value(optional, Opts) =/= true,
						IsProdOnly = case proplists:get_value(only, Opts, prod) of
							prod -> true;
							L when is_list(L) -> lists:member(prod, L);
							_ -> false
						end,
						case IsRequired andalso IsProdOnly of
							true when Path =/= undefined ->
								{
									[DEPS_Acc0, io_lib:format("DEPS += ~p~n", [Name])],
									[DEP_Acc0, io_lib:format("dep_~p = ln ~s~n", [Name, Path])]
								};
							true when Path =:= undefined ->
								io:format(standard_error, "Skipping 'dep_~p' as no vsn given.", [Name]),
								{
									[DEPS_Acc0, io_lib:format("DEPS += ~p~n", [Name])],
									DEP_Acc0
								};
							false ->
								{DEPS_Acc0, DEP_Acc0}
						end;
					{Name, Req, Opts} ->
						IsRequired = proplists:get_value(optional, Opts) =/= true,
						IsProdOnly = case proplists:get_value(only, Opts, prod) of
							prod -> true;
							L when is_list(L) -> lists:member(prod, L);
							_ -> false
						end,
						case IsRequired andalso IsProdOnly of
							true ->
								{
									[DEPS_Acc0, io_lib:format("DEPS += ~p~n", [Name])],
									[DEP_Acc0, io_lib:format("dep_~p = hex ~s ~p~n", [Name, HexVersionResolve(Name, Req), Name])]
								};
							false ->
								{DEPS_Acc0, DEP_Acc0}
						end;
					_ ->
						{DEPS_Acc0, DEP_Acc0}
				end,
			F(T, DEPS_Acc1, DEP_Acc1)
	end)(Deps, [], [])
end)($1)
endef

define Mix_Makefile.erl
{ok, _} = application:ensure_all_started(elixir),
{ok, _} = application:ensure_all_started(mix),
File = <<"$(DEPS_DIR)/$(1)/mix.exs">>,
[{Mod, Bin}] = elixir_compiler:file(File, fun(_File, _LexerPid) -> ok end),
{module, Mod} = code:load_binary(Mod, binary_to_list(File), Bin),
Project = Mod:project(),
Application = try Mod:application() catch error:undef -> [] end,
Fmt =
	"PROJECT = ~p~n"
	"PROJECT_DESCRIPTION = ~s~n"
	"PROJECT_VERSION = ~s~n"
	"PROJECT_MOD = ~s~n"
	"define PROJECT_ENV~n"
	"~p~n"
	"endef~n"
	"~n~n~s~n~n"
	"~n~n~s~n~n"
	"~n~s~n"
	"ERLC_OPTS = +debug_info~n"
	"include $$(if $$(ERLANG_MK_FILENAME),$$(ERLANG_MK_FILENAME),erlang.mk)",
LFirst = fun
	F([], Pred) -> 
		false;
	F([H|T], Pred) ->
		case catch Pred(H) of
			true ->
				{ok, H};
			false ->
				F(T, Pred)
		end
end,
StartMod =
	case lists:keyfind(mod, 1, Application) of
		{mod, {StartMod_, _StartArgs}} ->
			atom_to_list(StartMod_);
		_ ->
			""
	end,
ExtraApps = [io_lib:format("LOCAL_DEPS += ~p~n", [App]) || App <- proplists:get_value(extra_applications, Application, [])],
ProjectCompilers = proplists:get_value(compilers, Project, []),
"https://hexdocs.pm/elixir_make/Mix.Tasks.Compile.ElixirMake.html",
ExtraMakeLines = 
	case lists:member(elixir_make, ProjectCompilers) of
		false -> 
			"";
		true ->
			Fetch = fun(Key, Proplist, DefaultVal, DefaultReplacement) ->
				case proplists:get_value(Key, Proplist, DefaultVal) of
					DefaultVal -> DefaultReplacement;
					Value -> Value
				end
			end,
			case file:copy("$(DEPS_DIR)/$(1)/" ++ Fetch(make_makefile, Project, default, "Makefile"), "$(DEPS_DIR)/$(1)/elixir_make.mk") of
				{ok, _} -> ok;
				Err = {error, _} ->
					io:format(standard_error, "Failed to copy Makefile with error ~p~n", [Err]),
					halt(1)
			end,
			[
				io_lib:format("app::~n\t~s -C \"~s\" -f \"$(DEPS_DIR)/$(1)/elixir_make.mk\" ~s ~s~n", [
					Fetch(make_executable, Project, default, "$(MAKE)"),
					Fetch(make_cwd, Project, undefined, <<".">>),
					lists:join(" ", Fetch(make_targets, Project, [], [])),
					lists:join(" ", Fetch(make_args, Project, undefined, []))
				]),
				"\n",
				case Fetch(make_clean, Project, nil, undefined) of
					undefined -> 
						"";
					Clean ->
						io_lib:format("clean::~n\t~s~n", [Clean])
				end
			]
	end,
Deps = 
	lists:foldl(fun(DepToRemove, Acc) -> 
		lists:keydelete(DepToRemove, 1, Acc)
	end, proplists:get_value(deps, Project, []), [elixir_make]),
Args = [
	proplists:get_value(app, Project),
	proplists:get_value(description, Project, ""),
	proplists:get_value(version, Project, ""),
	StartMod,
	proplists:get_value(env, Application, []),
	ExtraApps,
	$(call elixir_get_deps.erl, Deps),
	ExtraMakeLines
],
Str = io_lib:format(Fmt, Args),
case file:write_file("$(DEPS_DIR)/$(1)/Makefile", Str) of
	ok -> 
		halt(0);
	{error, Reason} -> 
		io:format(standard_error, "Failed to create '$(DEPS_DIR)/$(1)/Makefile' with reason ~p~n", [Reason]),
		halt(1)
end
endef

define SHELL_ASSERT_
if ! $(1); then \
	echo "$(2)" >&2; \
	exit 1; \
fi
endef

define SHELL_ASSERT
$(if $(verbose),,$(info [SHELL_ASSERT] $(call SHELL_ASSERT_,$(1),$(2)))) $(call SHELL_ASSERT_,$(1),$(2))
endef

define dep_autopatch_mix
	$(MAKE) $(ELIXIR_PATH)/lib/elixir/ebin/elixir.app hex-core || exit 1; \
	sed 's|\(defmodule.*do\)|\1\ntry do\nCode.compiler_options(on_undefined_variable: :warn)\nrescue _ -> :ok\nend|g' -i $(DEPS_DIR)/$(1)/mix.exs; \
	MIX_ENV="$(if $(MIX_ENV),$(strip $(MIX_ENV)),prod)" \
		$(ERL) -pa $(DEPS_DIR)/hex_core $(addprefix -pa ,$(addsuffix /ebin,$(ELIXIR_BUILTINS))) \
		-eval "$(subst ",\",$(subst $(newline), ,$(subst $$,\$$,$(call Mix_Makefile.erl,$(1)))))." \
		-eval "halt(0)." || exit 1 \
	mkdir $(DEPS_DIR)/$1/src || exit 1
endef

ifneq ($(USES_ELIXIR),)
LOCAL_DEPS += eex elixir logger mix

ebin/$(PROJECT).app:: $(ELIXIR_PATH)/lib/elixir/ebin/elixir.app
endif

$(addsuffix /ebin,$(ELIXIR_BUILTINS)): $(ELIXIR_PATH)/lib/elixir/ebin/elixir.app
	$(verbose) $(if $(ELIXIR_USE_SYSTEM),@,$(MAKE) -C $(DEPS_DIR)/elixir IS_DEP=1)

define compile_ex.erl
	{ok, _} = application:ensure_all_started(elixir),
	ModCode = list_to_atom("Elixir.Code"),
	ModCode:put_compiler_option(ignore_module_conflict, true),
	ModComp = list_to_atom("Elixir.Kernel.ParallelCompiler"),
	{ok, Modules, _} = ModComp:compile_to_path([$(call comma_list,$(patsubst %,<<"%">>,$(EX_FILES)))], <<"ebin/">>),
	lists:foreach(fun(E) -> io:format("~p ", [E]) end, Modules),
	halt()
endef

$(ELIXIR_PATH)/lib/elixir/ebin/elixir.app: $(ELIXIR_PATH)
ifeq ($(ELIXIR_USE_SYSTEM),1)
	@
else
	$(verbose) $(MAKE) -C $(DEPS_DIR)/elixir -f Makefile.orig compile Q="$(verbose)"
endif

# We need the original makefile so that we can compile the elixir compiler
autopatch-elixir::
	$(verbose) cp $(DEPS_DIR)/elixir/Makefile $(DEPS_DIR)/elixir/Makefile.orig
	$(verbose) sed 's|"$$(MAKE)"|"$$(MAKE)" -f $$(CURDIR)/Makefile.orig|g' -i $(DEPS_DIR)/elixir/Makefile.orig

ifneq ($(USES_ELIXIR),)
ebin/$(PROJECT).app:: $(eval $(call dep_target,elixir)) $(addsuffix /ebin,$(ELIXIR_BUILTINS))
	@
endif
