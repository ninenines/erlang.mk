# Copyright (c) 2014-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: bootstrap bootstrap-lib bootstrap-rel new list-templates

# Core targets.

help::
	@printf "%s\n" "" \
		"Bootstrap targets:" \
		"  bootstrap          Generate a skeleton of an OTP application" \
		"  bootstrap-lib      Generate a skeleton of an OTP library" \
		"  bootstrap-rel      Generate the files needed to build a release" \
		"  new t=TPL n=NAME   Generate a module NAME based on the template TPL" \
		"  list-templates     List available templates"

# Bootstrap templates.

WS ?= $(tab)

define bs_appsrc
{application, $(PROJECT), [
$(WS){description, ""},
$(WS){vsn, "0.1.0"},
$(WS){id, "git"},
$(WS){modules, []},
$(WS){registered, []},
$(WS){applications, [
$(WS)$(WS)kernel,
$(WS)$(WS)stdlib
$(WS)]},
$(WS){mod, {$(PROJECT)_app, []}},
$(WS){env, []}
]}.
endef

define bs_appsrc_lib
{application, $(PROJECT), [
$(WS){description, ""},
$(WS){vsn, "0.1.0"},
$(WS){id, "git"},
$(WS){modules, []},
$(WS){registered, []},
$(WS){applications, [
$(WS)$(WS)kernel,
$(WS)$(WS)stdlib
$(WS)]}
]}.
endef

define bs_Makefile
PROJECT = $(PROJECT)
include erlang.mk
endef

define bs_app
-module($(PROJECT)_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
$(WS)$(PROJECT)_sup:start_link().

stop(_State) ->
$(WS)ok.
endef

define bs_relx_config
{release, {$(PROJECT)_release, "1"}, [$(PROJECT)]}.
{extended_start_script, true}.
{sys_config, "rel/sys.config"}.
{vm_args, "rel/vm.args"}.
endef

define bs_sys_config
[
].
endef

define bs_vm_args
-name $(PROJECT)@127.0.0.1
-setcookie $(PROJECT)
-heart
endef

# Normal templates.

define tpl_supervisor
-module($(n)).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
$(WS)supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
$(WS)Procs = [],
$(WS){ok, {{one_for_one, 1, 5}, Procs}}.
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
$(WS)gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
$(WS){ok, #state{}}.

handle_call(_Request, _From, State) ->
$(WS){reply, ignored, State}.

handle_cast(_Msg, State) ->
$(WS){noreply, State}.

handle_info(_Info, State) ->
$(WS){noreply, State}.

terminate(_Reason, _State) ->
$(WS)ok.

code_change(_OldVsn, State, _Extra) ->
$(WS){ok, State}.
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
$(WS){ok, Req, #state{}}.

handle(Req, State=#state{}) ->
$(WS){ok, Req2} = cowboy_req:reply(200, Req),
$(WS){ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
$(WS)ok.
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
$(WS)gen_fsm:start_link(?MODULE, [], []).

%% gen_fsm.

init([]) ->
$(WS){ok, state_name, #state{}}.

state_name(_Event, StateData) ->
$(WS){next_state, state_name, StateData}.

handle_event(_Event, StateName, StateData) ->
$(WS){next_state, StateName, StateData}.

state_name(_Event, _From, StateData) ->
$(WS){reply, ignored, state_name, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
$(WS){reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
$(WS){next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
$(WS)ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
$(WS){ok, StateName, StateData}.
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
$(WS){loop, Req, #state{}, 5000, hibernate}.

info(_Info, Req, State) ->
$(WS){loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
$(WS)ok.
endef

define tpl_cowboy_rest
-module($(n)).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_html/2]).

init(_, _Req, _Opts) ->
$(WS){upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
$(WS){[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State) ->
$(WS){<<"<html><body>This is REST!</body></html>">>, Req, State}.
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
$(WS){upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
$(WS)Req2 = cowboy_req:compact(Req),
$(WS){ok, Req2, #state{}}.

websocket_handle({text, Data}, Req, State) ->
$(WS){reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
$(WS){reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
$(WS){ok, Req, State}.

websocket_info(_Info, Req, State) ->
$(WS){ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
$(WS)ok.
endef

define tpl_ranch_protocol
-module($(n)).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-type opts() :: [].
-export_type([opts/0]).

-record(state, {
$(WS)socket :: inet:socket(),
$(WS)transport :: module()
}).

start_link(Ref, Socket, Transport, Opts) ->
$(WS)Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
$(WS){ok, Pid}.

-spec init(ranch:ref(), inet:socket(), module(), opts()) -> ok.
init(Ref, Socket, Transport, _Opts) ->
$(WS)ok = ranch:accept_ack(Ref),
$(WS)loop(#state{socket=Socket, transport=Transport}).

loop(State) ->
$(WS)loop(State).
endef

# Plugin-specific targets.

define render_template
	@echo "$${$(1)}" > $(2)
endef

$(foreach template,$(filter bs_%,$(.VARIABLES)),$(eval export $(template)))
$(foreach template,$(filter tpl_%,$(.VARIABLES)),$(eval export $(template)))

bootstrap:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(call render_template,bs_Makefile,Makefile)
	@mkdir src/
	$(call render_template,bs_appsrc,src/$(PROJECT).app.src)
	$(call render_template,bs_app,src/$(PROJECT)_app.erl)
	$(eval n := $(PROJECT)_sup)
	$(call render_template,tpl_supervisor,src/$(PROJECT)_sup.erl)

bootstrap-lib:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(call render_template,bs_Makefile,Makefile)
	@mkdir src/
	$(call render_template,bs_appsrc_lib,src/$(PROJECT).app.src)

bootstrap-rel:
ifneq ($(wildcard relx.config),)
	$(error Error: relx.config already exists)
endif
ifneq ($(wildcard rel/),)
	$(error Error: rel/ directory already exists)
endif
	$(call render_template,bs_relx_config,relx.config)
	@mkdir rel/
	$(call render_template,bs_sys_config,rel/sys.config)
	$(call render_template,bs_vm_args,rel/vm.args)

new:
ifeq ($(wildcard src/),)
	$(error Error: src/ directory does not exist)
endif
ifndef t
	$(error Usage: $(MAKE) new t=TEMPLATE n=NAME)
endif
ifndef tpl_$(t)
	$(error Unknown template)
endif
ifndef n
	$(error Usage: $(MAKE) new t=TEMPLATE n=NAME)
endif
	$(call render_template,tpl_$(t),src/$(n).erl)

list-templates:
	@echo Available templates: $(sort $(patsubst tpl_%,%,$(filter tpl_%,$(.VARIABLES))))
