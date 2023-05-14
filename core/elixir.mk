ifeq ($(pkg_elixir_commit),master)
pkg_elixir_commit = main
endif

ELIXIRC = $(DEPS_DIR)/$(call dep_name,elixir)/bin/elixirc

ELIXIR_COMPILE_FIRST_PATHS = $(addprefix src/,$(addsuffix .ex,$(COMPILE_FIRST))) $(addprefix lib/,$(addsuffix .ex,$(COMPILE_FIRST)))
ELIXIRC_EXCLUDE_PATHS = $(addprefix src/,$(addsuffix .ex,$(ERLC_EXCLUDE))) $(addprefix lib/,$(addsuffix .ex,$(ERLC_EXCLUDE)))
ELIXIRC_OPTS += $(ERLC_OPTS)

elixirc_verbose_0 = @echo " ELIXIRC  " $(filter-out $(patsubst %,%.ex,$(ERLC_EXCLUDE)),\
	$(filter %.ex %.core,$(?F)));
elixirc_verbose_2 = set -x;
elixirc_verbose = $(elixirc_verbose_$(V))

ALL_LIB_FILES := $(sort $(call core_find,lib/,*))

EX_FILES := $(filter %.ex,$(ALL_SRC_FILES) $(ALL_LIB_FILES))
ELIXIR_BUILTINS = $(addprefix $(DEPS_DIR)/$(call dep_name,elixir)/lib/,eex elixir logger mix)

define Mix_Makefile.erl
case "$(basename $(wildcard $(DEPS_DIR)/$(1)/.tmp/Elixir.*.beam))" =:= "" of
	true ->
		BeamFilter = "$(DEPS_DIR)/$(1)/.tmp/Elixir.*.beam",
		Wildcard = "$(wildcard $(DEPS_DIR)/$(1)/.tmp/Elixir.*.beam)",
		Basename = "$(basename $(wildcard $(DEPS_DIR)/$(1)/.tmp/Elixir.*.beam))",
		io:format(standard_error, "FAILED TO FIND BEAM!! BeamFilter: ~p, Wildcard: ~p, Basename: ~p~n", [BeamFilter, Wildcard, Basename]),
		halt(1);
	false ->
		ok
end,
{ok, _} = application:ensure_all_started(mix),
{module, Mod} = code:load_abs("$(basename $(wildcard $(DEPS_DIR)/$(1)/.tmp/Elixir.*.beam))"),
Project = Mod:project(),
Fmt =
	"PROJECT = ~p~n"
	"PROJECT_DESCRIPTION = ~s~n"
	"PROJECT_VERSION = ~s~n"
	"~n~n~s~n~n"
	"ERLC_OPTS = +debug_info~n"
	"include ../../erlang.mk",
GetDeps = 
	fun(Deps) ->
		CleanVer = 
			fun(Ver) ->
				Trim = fun(Bin) ->
					ShouldTrim = fun(C) -> C =:= $$\s orelse C =:= $$\t end,
					TrimLeft = 
						fun 
							F(<<>>) -> <<>>;
							F(Bin = <<C, Tail/binary>>) ->
								case ShouldTrim(C) of
									true ->
										F(Tail);
									false ->
										Bin
								end
						end,
					TrimRight =
						fun
							F(<<>>) ->
								<<>>;
							F(Bin) ->
								Len = byte_size(Bin),
								<<Left:(Len-1)/binary, C:8>> = Bin,
								case ShouldTrim(C) of
									true ->
										F(Left);
									false ->
										Bin
								end
						end,
					TrimLeft(TrimRight(Bin))
				end,
				CharList = [$$., $$\s | lists:seq($$0, $$9)],
				SmallVer = << <<C>> || <<C:8>> <= Ver, lists:member(C, CharList)>>,
				hd(binary:split(Trim(SmallVer), <<" ">>, [global]))
			end,
		(fun
			F([], DEPS_Acc0, DEP_Acc0) ->
				[DEPS_Acc0, "\n", DEP_Acc0];
			F([H|T], DEPS_Acc0, DEP_Acc0) ->
				{DEPS_Acc1, DEP_Acc1} =
					case H of
						{Name, Ver} when is_binary(Ver) ->
							{
								[DEPS_Acc0, io_lib:format("DEPS += ~p~n", [Name])],
								[DEP_Acc0, io_lib:format("dep_~p = hex ~s ~p~n", [Name, CleanVer(Ver), Name])]
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
						{Name, Ver, Opts} ->
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
										[DEP_Acc0, io_lib:format("dep_~p = hex ~s ~p~n", [Name, CleanVer(Ver), Name])]
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
Args = [
	proplists:get_value(app, Project),
	proplists:get_value(description, Project, ""),
	proplists:get_value(version, Project, ""),
	GetDeps((proplists:get_value(deps, Project, [])))
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
	$(MAKE) $(ELIXIRC); \
	echo "$(shell \
		$(call SHELL_ASSERT,sed 's|use Mix.Project||g' $(DEPS_DIR)/$1/mix.exs > $(DEPS_DIR)/$1/mix.exs.tmp,Failed to create mix.exs.tmp); \
		$(call SHELL_ASSERT,$(ELIXIRC) $(if $(elixirc_verbose),--verbose) --eval '{:ok$(comma) _} = :application.ensure_all_started(:mix);' $(DEPS_DIR)/$1/mix.exs.tmp -o $(DEPS_DIR)/$1/.tmp/,Failed to compile mix.exs.tmp); \
		$(call SHELL_ASSERT,rm -f $(DEPS_DIR)/$1/mix.exs.tmp,Failed to remove mix.exs.tmp); \
	)"; \
	MIX_ENV="$(if $(MIX_ENV),$(strip $(MIX_ENV)),prod)" $(ERL) $(addprefix -pa ,$(addsuffix /ebin,$(ELIXIR_BUILTINS))) \
		-eval "$(subst ",\",$(subst $(newline), ,$(subst $$,\$$,$(call Mix_Makefile.erl,$(1)))))." \
		-eval "halt(0)." || exit 1 \
	rm -rf $(DEPS_DIR)/$1/.tmp/ || exit 1 \
	mkdir $(DEPS_DIR)/$1/src || exit 1
endef

ifneq ($(EX_FILES),)
LOCAL_DEPS_DIRS += $(ELIXIR_BUILTINS)
LOCAL_DEPS += eex elixir logger mix

ebin/$(PROJECT).app:: $(ELIXIRC)
endif

$(ELIXIRC): $(DEPS_DIR)/$(call dep_name,elixir)/lib/elixir/ebin/elixir.app

$(addsuffix /ebin,$(ELIXIR_BUILTINS)): $(DEPS_DIR)/$(call dep_name,elixir)
	make -C $(DEPS_DIR)/elixir IS_DEP=1

define compile_ex
	ERL_COMPILER_OPTIONS="[$(call comma_list,$(patsubst '%',%,$(patsubst +%,%,$(filter +%,$(ELIXIRC_OPTS)))))]" $(ELIXIRC) \
		--verbose $(if $(IS_DEP),,$(if $(filter -Werror,$(ELIXIRC_OPTS)),--warnings-as-errors)) -o ebin/ $(filter-out $(ELIXIRC_EXCLUDE_PATHS),$(ELIXIR_COMPILE_FIRST_PATHS)) $(1) --  -pa ebin/ -I include/
endef

define get_elixir_mod
'Elixir.$(strip \
	$(patsubst %do,%,$(patsubst defmodule%,%,\
		$(shell grep 'defmodule' $(1))\
	))\
)'
endef

ebin/$(PROJECT).app:: $(EX_FILES)
	$(if $(strip $(EX_FILES)),$(call compile_ex,$(EX_FILES)))
# Older git versions do not have the --first-parent flag. Do without in that case.
	$(eval GITDESCRIBE := $(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null \
		|| git describe --dirty --abbrev=7 --tags --always 2>/dev/null || true))
	$(eval MODULES := $(patsubst %,'%',$(sort $(notdir $(basename \
		$(filter-out $(ELIXIRC_EXCLUDE_PATHS), $(ERL_FILES) $(CORE_FILES) $(BEAM_FILES)))))))
	$(eval MODULES := $(MODULES) $(foreach file, \
		$(EX_FILES), \
		$(call get_elixir_mod,$(file)) \
	))
ifeq ($(wildcard src/$(PROJECT).app.src),)
	$(app_verbose) printf '$(subst %,%%,$(subst $(newline),\n,$(subst ','\'',$(call app_file,$(GITDESCRIBE),$(MODULES)))))' \
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

$(DEPS_DIR)/$(call dep_name,elixir)/lib/elixir/ebin/elixir.app: $(DEPS_DIR)/$(call dep_name,elixir)
	$(MAKE) -C $(DEPS_DIR)/elixir -f Makefile.orig compile

# We need the original makefile so that we can compile the elixir compiler
autopatch-elixir::
	@cp $(DEPS_DIR)/elixir/Makefile $(DEPS_DIR)/elixir/Makefile.orig
	@sed 's|"$$(MAKE)"|"$$(MAKE)" -f $$(CURDIR)/Makefile.orig|g' -i $(DEPS_DIR)/elixir/Makefile.orig

$(EX_FILES): $(eval $(call dep_target,elixir)) $(addsuffix /ebin,$(ELIXIR_BUILTINS))