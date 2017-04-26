# Copyright (c) 2014-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: bootstrap bootstrap-lib bootstrap-rel new list-templates

# Core targets.

help::
	$(verbose) printf "%s\n" "" \
		"Bootstrap targets:" \
		"  bootstrap          Generate a skeleton of an OTP application" \
		"  bootstrap-lib      Generate a skeleton of an OTP library" \
		"  bootstrap-rel      Generate the files needed to build a release" \
		"  new-app in=NAME    Create a new local OTP application NAME" \
		"  new-lib in=NAME    Create a new local OTP library NAME" \
		"  new t=TPL n=NAME   Generate a module NAME based on the template TPL" \
		"  new t=T n=N in=APP Generate a module NAME based on the template TPL in APP" \
		"  list-templates     List available templates"

# Bootstrap templates.

define bs_appsrc
{application, $p, [
	{description, ""},
	{vsn, "0.1.0"},
	{id, "git"},
	{modules, []},
	{registered, []},
	{applications, [
		kernel,
		stdlib
	]},
	{mod, {$p_app, []}},
	{env, []}
]}.
endef

define bs_appsrc_lib
{application, $p, [
	{description, ""},
	{vsn, "0.1.0"},
	{id, "git"},
	{modules, []},
	{registered, []},
	{applications, [
		kernel,
		stdlib
	]}
]}.
endef

# To prevent autocompletion issues with ZSH, we add "include erlang.mk"
# separately during the actual bootstrap.
ifdef SP
define bs_Makefile
PROJECT = $p
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

# Whitespace to be used when creating files from templates.
SP = $(SP)

endef
else
define bs_Makefile
PROJECT = $p
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

endef
endif

define bs_apps_Makefile
PROJECT = $p
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

include $(call core_relpath,$(dir $(ERLANG_MK_FILENAME)),$(APPS_DIR)/app)/erlang.mk
endef

define bs_app
-module($p_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	$p_sup:start_link().

stop(_State) ->
	ok.
endef

define bs_relx_config
{release, {$p_release, "1"}, [$p, sasl, runtime_tools]}.
{extended_start_script, true}.
{sys_config, "rel/sys.config"}.
{vm_args, "rel/vm.args"}.
endef

define bs_sys_config
[
].
endef

define bs_vm_args
-name $p@127.0.0.1
-setcookie $p
-heart
endef

# Normal templates.

define tpl_supervisor
-module($(n)).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
endef

define tpl_gen_server
-module($(n)).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
endef

define tpl_module
-module($(n)).
-export([]).
endef

define tpl_cowboy_http
-module($(n)).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{ok, Req2} = cowboy_req:reply(200, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
endef

define tpl_gen_fsm
-module($(n)).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).

%% gen_fsm.
-export([init/1]).
-export([state_name/2]).
-export([handle_event/3]).
-export([state_name/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

%% gen_fsm.

init([]) ->
	{ok, state_name, #state{}}.

state_name(_Event, StateData) ->
	{next_state, state_name, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

state_name(_Event, _From, StateData) ->
	{reply, ignored, state_name, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.
endef

define tpl_cowboy_loop
-module($(n)).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{loop, Req, #state{}, 5000, hibernate}.

info(_Info, Req, State) ->
	{loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
	ok.
endef

define tpl_cowboy_rest
-module($(n)).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_html/2]).

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State) ->
	{<<"<html><body>This is REST!</body></html>">>, Req, State}.
endef

define tpl_cowboy_ws
-module($(n)).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
	Req2 = cowboy_req:compact(Req),
	{ok, Req2, #state{}}.

websocket_handle({text, Data}, Req, State) ->
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
endef

define tpl_ranch_protocol
-module($(n)).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-type opts() :: [].
-export_type([opts/0]).

-record(state, {
	socket :: inet:socket(),
	transport :: module()
}).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

-spec init(ranch:ref(), inet:socket(), module(), opts()) -> ok.
init(Ref, Socket, Transport, _Opts) ->
	ok = ranch:accept_ack(Ref),
	loop(#state{socket=Socket, transport=Transport}).

loop(State) ->
	loop(State).
endef

# Plugin-specific targets.

define render_template
	$(verbose) printf -- '$(subst $(newline),\n,$(subst %,%%,$(subst ','\'',$(subst $(tab),$(WS),$(call $(1))))))\n' > $(2)
endef

ifndef WS
ifdef SP
WS = $(subst a,,a $(wordlist 1,$(SP),a a a a a a a a a a a a a a a a a a a a))
else
WS = $(tab)
endif
endif

bootstrap:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(eval n := $(PROJECT)_sup)
	$(call render_template,bs_Makefile,Makefile)
	$(verbose) echo "include erlang.mk" >> Makefile
	$(verbose) mkdir src/
ifdef LEGACY
	$(call render_template,bs_appsrc,src/$(PROJECT).app.src)
endif
	$(call render_template,bs_app,src/$(PROJECT)_app.erl)
	$(call render_template,tpl_supervisor,src/$(PROJECT)_sup.erl)

bootstrap-lib:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(call render_template,bs_Makefile,Makefile)
	$(verbose) echo "include erlang.mk" >> Makefile
	$(verbose) mkdir src/
ifdef LEGACY
	$(call render_template,bs_appsrc_lib,src/$(PROJECT).app.src)
endif

bootstrap-rel:
ifneq ($(wildcard relx.config),)
	$(error Error: relx.config already exists)
endif
ifneq ($(wildcard rel/),)
	$(error Error: rel/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(call render_template,bs_relx_config,relx.config)
	$(verbose) mkdir rel/
	$(call render_template,bs_sys_config,rel/sys.config)
	$(call render_template,bs_vm_args,rel/vm.args)

new-app:
ifndef in
	$(error Usage: $(MAKE) new-app in=APP)
endif
ifneq ($(wildcard $(APPS_DIR)/$in),)
	$(error Error: Application $in already exists)
endif
	$(eval p := $(in))
	$(eval n := $(in)_sup)
	$(verbose) mkdir -p $(APPS_DIR)/$p/src/
	$(call render_template,bs_apps_Makefile,$(APPS_DIR)/$p/Makefile)
ifdef LEGACY
	$(call render_template,bs_appsrc,$(APPS_DIR)/$p/src/$p.app.src)
endif
	$(call render_template,bs_app,$(APPS_DIR)/$p/src/$p_app.erl)
	$(call render_template,tpl_supervisor,$(APPS_DIR)/$p/src/$p_sup.erl)

new-lib:
ifndef in
	$(error Usage: $(MAKE) new-lib in=APP)
endif
ifneq ($(wildcard $(APPS_DIR)/$in),)
	$(error Error: Application $in already exists)
endif
	$(eval p := $(in))
	$(verbose) mkdir -p $(APPS_DIR)/$p/src/
	$(call render_template,bs_apps_Makefile,$(APPS_DIR)/$p/Makefile)
ifdef LEGACY
	$(call render_template,bs_appsrc_lib,$(APPS_DIR)/$p/src/$p.app.src)
endif

new:
ifeq ($(wildcard src/)$(in),)
	$(error Error: src/ directory does not exist)
endif
ifndef t
	$(error Usage: $(MAKE) new t=TEMPLATE n=NAME [in=APP])
endif
ifndef n
	$(error Usage: $(MAKE) new t=TEMPLATE n=NAME [in=APP])
endif
ifdef in
	$(verbose) $(MAKE) -C $(APPS_DIR)/$(in)/ new t=$t n=$n in=
else
	$(call render_template,tpl_$(t),src/$(n).erl)
endif

list-templates:
	$(verbose) echo Available templates: $(sort $(patsubst tpl_%,%,$(filter tpl_%,$(.VARIABLES))))
