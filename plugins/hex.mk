# Copyright (c) 2020, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# We automatically depend on hex_core when the project isn't already.
$(if $(filter hex_core,$(DEPS) $(BUILD_DEPS) $(DOC_DEPS) $(REL_DEPS) $(TEST_DEPS)),,\
	$(eval $(call dep_target,hex_core)))

hex-core: $(DEPS_DIR)/hex_core
	$(verbose) if [ ! -e $(DEPS_DIR)/hex_core/ebin/dep_built ]; then \
		$(MAKE) -C $(DEPS_DIR)/hex_core IS_DEP=1; \
		touch $(DEPS_DIR)/hex_core/ebin/dep_built; \
	fi

# @todo This must also apply to fetching.
HEX_CONFIG ?=

define hex_config.erl
	begin
		Config0 = hex_core:default_config(),
		Config0$(HEX_CONFIG)
	end
endef

define hex_user_create.erl
	{ok, _} = application:ensure_all_started(ssl),
	{ok, _} = application:ensure_all_started(inets),
	Config = $(hex_config.erl),
	case hex_api_user:create(Config, <<"$(strip $1)">>, <<"$(strip $2)">>, <<"$(strip $3)">>) of
		{ok, {201, _, #{<<"email">> := Email, <<"url">> := URL, <<"username">> := Username}}} ->
			io:format("User ~s (~s) created at ~s~n"
				"Please check your inbox for a confirmation email.~n"
				"You must confirm before you are allowed to publish packages.~n",
				[Username, Email, URL]),
			halt(0);
		{ok, {Status, _, Errors}} ->
			io:format("Error ~b: ~0p~n", [Status, Errors]),
			halt(80)
	end
endef

# The $(info ) call inserts a new line after the password prompt.
hex-user-create: hex-core
	$(if $(HEX_USERNAME),,$(eval HEX_USERNAME := $(shell read -p "Username: " username; echo $$username)))
	$(if $(HEX_PASSWORD),,$(eval HEX_PASSWORD := $(shell stty -echo; read -p "Password: " password; stty echo; echo $$password) $(info )))
	$(if $(HEX_EMAIL),,$(eval HEX_EMAIL := $(shell read -p "Email: " email; echo $$email)))
	$(gen_verbose) $(call erlang,$(call hex_user_create.erl,$(HEX_USERNAME),$(HEX_PASSWORD),$(HEX_EMAIL)))

define hex_key_add.erl
	{ok, _} = application:ensure_all_started(ssl),
	{ok, _} = application:ensure_all_started(inets),
	Config = $(hex_config.erl),
	ConfigF = Config#{api_key => iolist_to_binary([<<"Basic ">>, base64:encode(<<"$(strip $1):$(strip $2)">>)])},
	Permissions = [
		case string:split(P, <<":">>) of
			[D] -> #{domain => D};
			[D, R] -> #{domain => D, resource => R}
		end
	|| P <- string:split(<<"$(strip $4)">>, <<",">>, all)],
	case hex_api_key:add(ConfigF, <<"$(strip $3)">>, Permissions) of
		{ok, {201, _, #{<<"secret">> := Secret}}} ->
			io:format("Key ~s created for user ~s~nSecret: ~s~n"
				"Please store the secret in a secure location, such as a password store.~n"
				"The secret will be requested for most Hex-related operations.~n",
				[<<"$(strip $3)">>, <<"$(strip $1)">>, Secret]),
			halt(0);
		{ok, {Status, _, Errors}} ->
			io:format("Error ~b: ~0p~n", [Status, Errors]),
			halt(81)
	end
endef

hex-key-add: hex-core
	$(if $(HEX_USERNAME),,$(eval HEX_USERNAME := $(shell read -p "Username: " username; echo $$username)))
	$(if $(HEX_PASSWORD),,$(eval HEX_PASSWORD := $(shell stty -echo; read -p "Password: " password; stty echo; echo $$password) $(info )))
	$(gen_verbose) $(call erlang,$(call hex_key_add.erl,$(HEX_USERNAME),$(HEX_PASSWORD),\
		$(if $(name),$(name),$(shell hostname)-erlang-mk),\
		$(if $(perm),$(perm),api)))

HEX_TARBALL_EXTRA_METADATA ?=

# @todo Check that we can += files
# @todo Probably better if we sort the core_find results.
HEX_TARBALL_FILES ?= \
	$(wildcard early-plugins.mk) \
	$(wildcard ebin/$(PROJECT).app) \
	$(wildcard ebin/$(PROJECT).appup) \
	$(wildcard $(notdir $(ERLANG_MK_FILENAME))) \
	$(call core_find,include/,*.hrl) \
	$(wildcard LICENSE*) \
	$(wildcard Makefile) \
	$(wildcard plugins.mk) \
	$(call core_find,priv/,*) \
	$(wildcard README*) \
	$(wildcard rebar.config) \
	$(call core_find,src/,*)

HEX_TARBALL_OUTPUT_FILE ?= $(ERLANG_MK_TMP)/$(PROJECT).tar

# @todo Need to check for rebar.config and/or the absence of DEPS to know
# whether a project will work with Rebar.
#
# @todo contributors licenses links in HEX_TARBALL_EXTRA_METADATA

# In order to build the requirements metadata we look into DEPS.
# We do not require that the project use Hex dependencies, however
# Hex.pm does require that the package name and version numbers
# correspond to a real Hex package.
define hex_tarball_create.erl
	Files0 = [$(call comma_list,$(patsubst %,"%",$(HEX_TARBALL_FILES)))],
	Requirements0 = #{
		$(foreach d,$(DEPS),
			<<"$(if $(subst hex,,$(call query_fetch_method,$d)),$d,$(if $(word 3,$(dep_$d)),$(word 3,$(dep_$d)),$d))">> => #{
				<<"app">> => <<"$d">>,
				<<"optional">> => false,
				<<"requirement">> => <<"$(call query_version,$d)">>
			},)
		$(if $(DEPS),dummy => dummy)
	},
	Requirements = maps:remove(dummy, Requirements0),
	Metadata0 = #{
		app => <<"$(strip $(PROJECT))">>,
		build_tools => [<<"make">>, <<"rebar3">>],
		description => <<"$(strip $(PROJECT_DESCRIPTION))">>,
		files => [unicode:characters_to_binary(F) || F <- Files0],
		name => <<"$(strip $(PROJECT))">>,
		requirements => Requirements,
		version => <<"$(strip $(PROJECT_VERSION))">>
	},
	Metadata = Metadata0$(HEX_TARBALL_EXTRA_METADATA),
	Files = [case file:read_file(F) of
		{ok, Bin} ->
			{F, Bin};
		{error, Reason} ->
			io:format("Error trying to open file ~0p: ~0p~n", [F, Reason]),
			halt(82)
	end || F <- Files0],
	case hex_tarball:create(Metadata, Files) of
		{ok, #{tarball := Tarball}} ->
			ok = file:write_file("$(strip $(HEX_TARBALL_OUTPUT_FILE))", Tarball),
			halt(0);
		{error, Reason} ->
			io:format("Error ~0p~n", [Reason]),
			halt(83)
	end
endef

hex_tar_verbose_0 = @echo " TAR    $(notdir $(ERLANG_MK_TMP))/$(@F)";
hex_tar_verbose_2 = set -x;
hex_tar_verbose = $(hex_tar_verbose_$(V))

$(HEX_TARBALL_OUTPUT_FILE): hex-core app
	$(hex_tar_verbose) $(call erlang,$(call hex_tarball_create.erl))

hex-tarball-create: $(HEX_TARBALL_OUTPUT_FILE)

define hex_release_publish_summary.erl
	{ok, Tarball} = erl_tar:open("$(strip $(HEX_TARBALL_OUTPUT_FILE))", [read]),
	ok = erl_tar:extract(Tarball, [{cwd, "$(ERLANG_MK_TMP)"}, {files, ["metadata.config"]}]),
	{ok, Metadata} = file:consult("$(ERLANG_MK_TMP)/metadata.config"),
	#{
		<<"name">> := Name,
		<<"version">> := Version,
		<<"files">> := Files,
		<<"requirements">> := Deps
	} = maps:from_list(Metadata),
	io:format("Publishing ~s ~s~n  Dependencies:~n", [Name, Version]),
	case Deps of
		[] ->
			io:format("    (none)~n");
		_ ->
			[begin
				#{<<"app">> := DA, <<"requirement">> := DR} = maps:from_list(D),
				io:format("    ~s ~s~n", [DA, DR])
			end || {_, D} <- Deps]
	end,
	io:format("  Included files:~n"),
	[io:format("    ~s~n", [F]) || F <- Files],
	io:format("You may also review the contents of the tarball file.~n"
		"Please enter your secret key to proceed.~n"),
	halt(0)
endef

define hex_release_publish.erl
	{ok, _} = application:ensure_all_started(ssl),
	{ok, _} = application:ensure_all_started(inets),
	Config = $(hex_config.erl),
	ConfigF = Config#{api_key => <<"$(strip $1)">>},
	{ok, Tarball} = file:read_file("$(strip $(HEX_TARBALL_OUTPUT_FILE))"),
	case hex_api_release:publish(ConfigF, Tarball, [{replace, $2}]) of
		{ok, {200, _, #{}}} ->
			io:format("Release replaced~n"),
			halt(0);
		{ok, {201, _, #{}}} ->
			io:format("Release published~n"),
			halt(0);
		{ok, {Status, _, Errors}} ->
			io:format("Error ~b: ~0p~n", [Status, Errors]),
			halt(84)
	end
endef

hex-release-tarball: hex-core $(HEX_TARBALL_OUTPUT_FILE)
	$(verbose) $(call erlang,$(call hex_release_publish_summary.erl))

hex-release-publish: hex-core hex-release-tarball
	$(if $(HEX_SECRET),,$(eval HEX_SECRET := $(shell stty -echo; read -p "Secret: " secret; stty echo; echo $$secret) $(info )))
	$(gen_verbose) $(call erlang,$(call hex_release_publish.erl,$(HEX_SECRET),false))

hex-release-replace: hex-core hex-release-tarball
	$(if $(HEX_SECRET),,$(eval HEX_SECRET := $(shell stty -echo; read -p "Secret: " secret; stty echo; echo $$secret) $(info )))
	$(gen_verbose) $(call erlang,$(call hex_release_publish.erl,$(HEX_SECRET),true))

define hex_release_delete.erl
	{ok, _} = application:ensure_all_started(ssl),
	{ok, _} = application:ensure_all_started(inets),
	Config = $(hex_config.erl),
	ConfigF = Config#{api_key => <<"$(strip $1)">>},
	case hex_api_release:delete(ConfigF, <<"$(strip $(PROJECT))">>, <<"$(strip $(PROJECT_VERSION))">>) of
		{ok, {204, _, _}} ->
			io:format("Release $(strip $(PROJECT_VERSION)) deleted~n"),
			halt(0);
		{ok, {Status, _, Errors}} ->
			io:format("Error ~b: ~0p~n", [Status, Errors]),
			halt(85)
	end
endef

hex-release-delete: hex-core
	$(if $(HEX_SECRET),,$(eval HEX_SECRET := $(shell stty -echo; read -p "Secret: " secret; stty echo; echo $$secret) $(info )))
	$(gen_verbose) $(call erlang,$(call hex_release_delete.erl,$(HEX_SECRET)))

define hex_release_retire.erl
	{ok, _} = application:ensure_all_started(ssl),
	{ok, _} = application:ensure_all_started(inets),
	Config = $(hex_config.erl),
	ConfigF = Config#{api_key => <<"$(strip $1)">>},
	Params = #{<<"reason">> => <<"$(strip $3)">>, <<"message">> => <<"$(strip $4)">>},
	case hex_api_release:retire(ConfigF, <<"$(strip $(PROJECT))">>, <<"$(strip $2)">>, Params) of
		{ok, {204, _, _}} ->
			io:format("Release $(strip $2) has been retired~n"),
			halt(0);
		{ok, {Status, _, Errors}} ->
			io:format("Error ~b: ~0p~n", [Status, Errors]),
			halt(86)
	end
endef

hex-release-retire: hex-core
	$(if $(HEX_SECRET),,$(eval HEX_SECRET := $(shell stty -echo; read -p "Secret: " secret; stty echo; echo $$secret) $(info )))
	$(gen_verbose) $(call erlang,$(call hex_release_retire.erl,$(HEX_SECRET),\
		$(if $(HEX_VERSION),$(HEX_VERSION),$(PROJECT_VERSION)),\
		$(if $(HEX_REASON),$(HEX_REASON),invalid),\
		$(HEX_MESSAGE)))

define hex_release_unretire.erl
	{ok, _} = application:ensure_all_started(ssl),
	{ok, _} = application:ensure_all_started(inets),
	Config = $(hex_config.erl),
	ConfigF = Config#{api_key => <<"$(strip $1)">>},
	case hex_api_release:unretire(ConfigF, <<"$(strip $(PROJECT))">>, <<"$(strip $2)">>) of
		{ok, {204, _, _}} ->
			io:format("Release $(strip $2) is not retired anymore~n"),
			halt(0);
		{ok, {Status, _, Errors}} ->
			io:format("Error ~b: ~0p~n", [Status, Errors]),
			halt(87)
	end
endef

hex-release-unretire: hex-core
	$(if $(HEX_SECRET),,$(eval HEX_SECRET := $(shell stty -echo; read -p "Secret: " secret; stty echo; echo $$secret) $(info )))
	$(gen_verbose) $(call erlang,$(call hex_release_unretire.erl,$(HEX_SECRET),\
		$(if $(HEX_VERSION),$(HEX_VERSION),$(PROJECT_VERSION))))
