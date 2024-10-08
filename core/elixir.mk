ifeq ($(pkg_elixir_commit),master)
pkg_elixir_commit = main
endif

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

ELIXIR_COMPILE_FIRST_PATHS = $(addprefix src/,$(addsuffix .ex,$(COMPILE_FIRST))) $(addprefix lib/,$(addsuffix .ex,$(COMPILE_FIRST)))
ELIXIRC_EXCLUDE_PATHS = $(addprefix src/,$(addsuffix .ex,$(ERLC_EXCLUDE))) $(addprefix lib/,$(addsuffix .ex,$(ERLC_EXCLUDE)))
ELIXIRC_OPTS += $(ERLC_OPTS)

elixirc_verbose_0 = @echo " ELIXIRC  " $(filter-out $(patsubst %,%.ex,$(ERLC_EXCLUDE)),\
	$(filter %.ex %.core,$(?F)));
elixirc_verbose_2 = set -x;
elixirc_verbose = $(elixirc_verbose_$(V))

ALL_LIB_FILES := $(sort $(call core_find,lib/,*))

EX_FILES := $(filter-out lib/mix/%,$(filter %.ex,$(ALL_SRC_FILES) $(ALL_LIB_FILES)))
ELIXIR_BUILTINS = $(addprefix $(ELIXIR_PATH)/lib/,eex elixir logger mix)
USES_ELIXIR = $(if $(EX_FILES)$(shell find $(DEPS_DIR) -name '*.ex' 2>/dev/null),1,)

ifneq ($(USES_ELIXIR),)
ERL_LIBS := $(ERL_LIBS):$(ELIXIR_PATH)/lib/
export ERL_LIBS

define app_file
{application, '$(PROJECT)', [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},$(if $(IS_DEP),
	{id$(comma)$(space)"$(1)"}$(comma))
	{modules, [$(call comma_list,$(2))]},
	{registered, [$(call comma_list,$(PROJECT)_sup $(PROJECT_REGISTERED))]},
	{applications, [$(call comma_list,kernel stdlib $(OTP_DEPS) $(LOCAL_DEPS) $(foreach dep,$(DEPS),$(call dep_name,$(dep))))]},
	$(if $(PROJECT_MOD),{mod$(comma) {'$(PROJECT_MOD)'$(comma) []}}$(comma),)
	{env, $(subst \,\\,$(PROJECT_ENV))}$(if $(findstring {,$(PROJECT_APP_EXTRA_KEYS)),$(comma)$(newline)$(tab)$(subst \,\\,$(PROJECT_APP_EXTRA_KEYS)),)
]}.
endef

app:: $(if $(wildcard ebin/test),clean) deps
	$(verbose) $(MAKE) --no-print-directory $(PROJECT).d
	$(verbose) $(MAKE) --no-print-directory app-build

app-build:: ebin/$(PROJECT).app
	$(verbose) :

$(PROJECT).d:: ebin/$(PROJECT).app

define validate_app_file
	case file:consult("ebin/$(PROJECT).app") of
		{ok, _} -> halt();
		_ -> halt(1)
	end
endef
endif

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
	"include ../../erlang.mk",
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
GetDeps = 
	fun(Deps) ->
		GetVer = fun(Name, Req) ->
			application:ensure_all_started(ssl),
			application:ensure_all_started(inets),
			{ok, PackageInfo} =
				case hex_repo:get_package(hex_core:default_config(), atom_to_binary(Name)) of
					{ok, {200, _RespHeaders, Decoded}} ->
						LFirst(Decoded, fun(#{version := Vsn}) -> 'Elixir.Version':'match?'(Vsn, Req) end);
					Other ->
						io:format(standard_error, "Unexpected response for Dep ~p", [Name]),
						erlang:halt(1)
				end,
			maps:get(version, PackageInfo)
		end,
		(fun
			F([], DEPS_Acc0, DEP_Acc0) ->
				[DEPS_Acc0, "\n", DEP_Acc0];
			F([H|T], DEPS_Acc0, DEP_Acc0) ->
				{DEPS_Acc1, DEP_Acc1} =
					case H of
						{Name, Req} when is_binary(Req) ->
							{
								[DEPS_Acc0, io_lib:format("DEPS += ~p~n", [Name])],
								[DEP_Acc0, io_lib:format("dep_~p = hex ~s ~p~n", [Name, GetVer(Name, Req), Name])]
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
										[DEP_Acc0, io_lib:format("dep_~p = hex ~s ~p~n", [Name, GetVer(Name, Req), Name])]
									};
								false ->
									{DEPS_Acc0, DEP_Acc0}
							end;
						_ ->
							{DEPS_Acc0, DEP_Acc0}
					end,
				F(T, DEPS_Acc1, DEP_Acc1)
		end)(Deps, [], [])
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
	GetDeps(Deps),
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
	$(MAKE) $(ELIXIRC) hex-core; \
	sed 's|\(defmodule.*do\)|\1\nCode.compiler_options(on_undefined_variable: :warn)\n|g' -i $(DEPS_DIR)/$(1)/mix.exs; \
	MIX_ENV="$(if $(MIX_ENV),$(strip $(MIX_ENV)),prod)" $(ERL) -pa $(DEPS_DIR)/hex_core $(addprefix -pa ,$(addsuffix /ebin,$(ELIXIR_BUILTINS))) \
		-eval "$(subst ",\",$(subst $(newline), ,$(subst $$,\$$,$(call Mix_Makefile.erl,$(1)))))." \
		-eval "halt(0)." || exit 1 \
	mkdir $(DEPS_DIR)/$1/src || exit 1
endef

ifneq ($(USES_ELIXIR),)
LOCAL_DEPS += eex elixir logger mix

ebin/$(PROJECT).app:: $(ELIXIR_PATH)/lib/elixir/ebin/elixir.app
endif

$(ELIXIRC): $(ELIXIR_PATH)/lib/elixir/ebin/elixir.app

$(addsuffix /ebin,$(ELIXIR_BUILTINS)): $(ELIXIR_PATH)/lib/elixir/ebin/elixir.app
	$(verbose) $(if $(ELIXIR_USE_SYSTEM),@,$(MAKE) -C $(DEPS_DIR)/elixir IS_DEP=1D)

define compile_ex
ERL_COMPILER_OPTIONS="[$(call comma_list,$(patsubst '%',%,$(patsubst +%,%,$(filter +%,$(ELIXIRC_OPTS)))))]" $(ELIXIRC) \
	--verbose $(if $(IS_DEP),,$(if $(filter -Werror,$(ELIXIRC_OPTS)),--warnings-as-errors)) -o ebin/ $(filter-out $(ELIXIRC_EXCLUDE_PATHS),$(ELIXIR_COMPILE_FIRST_PATHS)) $(1) --  -pa ebin/ -I include/
endef

# Currently doesn't account for nested modules
define get_elixir_mod
$(foreach module,$(strip \
	$(patsubst %do,%,$(patsubst defmodule%,%,\
		$(shell grep 'defmodule' $(1))\
	))),'Elixir.$(module)')
endef

ebin/$(PROJECT).app:: $(EX_FILES)
	$(gen_verbose) $(if $(strip $(EX_FILES)),$(call compile_ex,$(EX_FILES)))
# Older git versions do not have the --first-parent flag. Do without in that case.
	$(eval GITDESCRIBE := $(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null \
		|| git describe --dirty --abbrev=7 --tags --always 2>/dev/null || true))
	$(eval MODULES := $(patsubst %,'%',$(sort $(notdir $(basename \
		$(filter-out $(ELIXIRC_EXCLUDE_PATHS), $(ERL_FILES) $(CORE_FILES) $(BEAM_FILES)))))))
	$(eval MODULES := $(foreach file, \
		$(EX_FILES), \
		$(call get_elixir_mod,$(file)) \
	))
ifeq ($(wildcard src/$(PROJECT).app.src),)
	$(app_verbose) printf "$(subst %,%%,$(subst $(newline),\n,$(subst ",\",$(call app_file,$(GITDESCRIBE),$(MODULES)))))" \
		> ebin/$(PROJECT).app
	$(verbose) if ! $(call erlang,$(call validate_app_file)); then \
		echo "The .app file produced is invalid. Please verify the value of PROJECT_ENV." >&2; \
		exit 1; \
	fi
else
	$(verbose) if [ -z "$$(grep -e '^[^%]*{\s*modules\s*,' src/$(PROJECT).app.src)" ]; then \
		echo "Empty modules entry not found in $(PROJECT).app.src. Please consult the erlang.mk documentation for instructions." >&2; \
		exit 1; \
	fi
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed "s/{[[:space:]]*modules[[:space:]]*,[[:space:]]*\[\]}/{modules, \[$(call comma_list,$(MODULES))\]}/" \
		| sed "s/{id,[[:space:]]*\"git\"}/{id, \"$(subst /,\/,$(GITDESCRIBE))\"}/" \
		> ebin/$(PROJECT).app
endif
ifneq ($(wildcard src/$(PROJECT).appup),)
	$(verbose) cp src/$(PROJECT).appup ebin/
endif

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
