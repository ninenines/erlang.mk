# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2014, Viktor Söderqvist <viktor@zuiderkwast.se>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# ZSH users have a more modern shell which doesn't need to
# have the same safeguards as other shells. To use ZSH instead
# of the default shell, set ZSH=1.

ifdef ZSH
SHELL := $(shell which zsh)
endif

# Temporary application name, taken from rule name.

APP = test_$(subst -,_,$@)

# Erlang, quickly!

ERL = erl +A0 -noinput -boot start_clean

# Platform detection, condensed version.

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
PLATFORM = darwin
else ifeq ($(UNAME_S),FreeBSD)
PLATFORM = freebsd
else ifeq ($(shell uname -o),Msys)
PLATFORM = msys2
else
PLATFORM = unix
endif

# Some systems do not have sub-second file times resolution.
# This is the case for older systems like OSX that uses the HFS+
# file system. HFS+ has a 1 second time resolution. This is a
# problem because the Erlang.mk tests rely on file modification
# times to ensure files were rebuilt. To fix this issue, we
# detect here whether the system supports sub-second resolution,
# and maybe sleep during test execution.
#
# Also see:
# * http://arstechnica.com/apple/2011/07/mac-os-x-10-7/12/#hfs-problems
# * https://apple.stackexchange.com/questions/51650/linus-torvalds-and-the-os-x-filesystem

ifeq ($(shell touch a; sleep 0.01; touch b; sleep 0.01; touch c; test c -nt b -a b -nt a; echo $$?; rm a b c),1)
SLEEP = sleep 1
else
SLEEP =
endif

# OTP master, for downloading files for testing.

OTP_MASTER = https://raw.githubusercontent.com/erlang/otp/master

# Verbosity.
#
# V=0: Show info messages only.
# V=1: Show test commands.
# V=2: Also show normal Erlang.mk output.
# V=3: Also show verbose Erlang.mk output.
# V=4: Also show a trace of each command after expansion.

V ?= 0

# t: Verbosity control for tests.
# v: Verbosity control for erlang.mk.
# i: Command to display (or suppress) info messages.

ifeq ($V,0)
	t = @
	v = V=0 >/dev/null 2>&1
	i = @echo $@:
else ifeq ($V,1)
	t =
	v = V=0 >/dev/null 2>&1
	i = @echo == $@:
else ifeq ($V,2)
	t = @echo " TEST  " $@;
	v = V=0
	i = @echo == $@:
else
	t =
	v = V=$(shell echo $$(($(V)-2)))
	i = @echo == $@:
endif

# Main targets.

.PHONY: all clean build

all:: core

clean::
	$t rm -rf erl_crash.dump packages/ test_*/

build:
	$i "Generate a bleeding edge Erlang.mk"
	$t cd .. && $(MAKE) $v

# Core.

.PHONY: core

define include_core
core:: core-$1

include core_$1.mk

endef

$(eval $(foreach t,$(patsubst %.mk,%,$(patsubst core_%,%,$(wildcard core_*.mk))),$(call include_core,$t)))

# Plugins.

define include_plugin
all:: $1

include plugin_$1.mk

endef

$(eval $(foreach t,$(patsubst %.mk,%,$(patsubst plugin_%,%,$(wildcard plugin_*.mk))),$(call include_plugin,$t)))

# Packages.

PACKAGES = $(foreach pkg,$(sort $(wildcard ../index/*.mk)),$(notdir $(basename $(pkg))))
EXCLUDE_FROM_CHECK = ['ci.erlang.mk', hexer_mk, inaka_mk, rabbitmq_codegen]

packages: $(addprefix pkg-,$(PACKAGES))

define pkg_target
.PHONY: pkg-$1

pkg-$1: build clean

# Make sure $@ is defined inside the define.
	$(eval @ = pkg-$1)

# Get the real application's name.
	$(eval APP_NAME := $(shell sed '2!d;s/pkg_$1_name = //' ../index/$1.mk))

	$i "Bootstrap a new OTP library in packages/$1_pkg"
	$t mkdir -p packages/$1_pkg/
	$t cp ../erlang.mk packages/$1_pkg/
	$t cd packages/$1_pkg/ && $(MAKE) -f erlang.mk bootstrap-lib $v

	$i "Add package $1 to the Makefile"
	$t perl -ni.bak -e 'print;if ($$$$.==1) {print "DEPS = $1\n"}' packages/$1_pkg/Makefile

	$i "Compile package $1"
	$t if ! ( cd packages/$1_pkg/ && $(MAKE) $(PATCHES) $v ); then \
		echo "$1: compile error" >> packages/errors.log; \
		false; \
	fi

	$i "Check that $1 has a .app file"
	$t if ! test -f packages/$1_pkg/deps/$(APP_NAME)/ebin/$(APP_NAME).app; then \
		echo "$1: no .app file" >> packages/errors.log; \
		false; \
	fi

	$i "Check that all applications and their modules can be loaded"
	$t if ! ( cd packages/$1_pkg/ && $(ERL) -pa deps/*/ebin/ -eval " \
		Apps0 = [list_to_atom(App) || \"deps/\" ++ App \
			<- filelib:wildcard(\"deps/*\")], \
		Apps = [App || App <- Apps0, not lists:member(App, $(EXCLUDE_FROM_CHECK))], \
		[begin \
			io:format(\"Loading application ~p~n\", [App]), \
			case application:load(App) of \
				ok -> ok; \
				{error, {already_loaded, App}} -> ok \
			end, \
			{ok, Mods} = application:get_key(App, modules), \
			[try io:format(\"  Loading module ~p~n\", [Mod]), \
				{module, Mod} = code:load_file(Mod) \
			catch C:R -> timer:sleep(500), erlang:C(R) \
			end || Mod <- Mods] \
		end || App <- Apps], \
		halt()." ); then \
			echo "$1: load error" >> packages/errors.log; \
			false; \
	fi

	$i "Recompile package $1"
	$t if ! ( cd packages/$1_pkg/ && $(MAKE) $(PATCHES) $v ); then \
		echo "$(1): recompile error" >> packages/errors.log; \
		false; \
	fi

	$i "Check that $1 has a .app file"
	$t if ! test -f packages/$1_pkg/deps/$(APP_NAME)/ebin/$(APP_NAME).app; then \
		echo "$1: no .app file" >> packages/errors.log; \
		false; \
	fi

	$i "Check that all applications and their modules can still be loaded"
	$t if ! ( cd packages/$1_pkg/ && $(ERL) -pa deps/*/ebin/ -eval " \
		Apps0 = [list_to_atom(App) || \"deps/\" ++ App \
			<- filelib:wildcard(\"deps/*\")], \
		Apps = [App || App <- Apps0, not lists:member(App, $(EXCLUDE_FROM_CHECK))], \
		[begin \
			io:format(\"Loading application ~p~n\", [App]), \
			case application:load(App) of \
				ok -> ok; \
				{error, {already_loaded, App}} -> ok \
			end, \
			{ok, Mods} = application:get_key(App, modules), \
			[try io:format(\"  Loading module ~p~n\", [Mod]), \
				{module, Mod} = code:load_file(Mod) \
			catch C:R -> timer:sleep(500), erlang:C(R) \
			end || Mod <- Mods] \
		end || App <- Apps], \
		halt()." ); then \
			echo "$1: recompile+load error" >> packages/errors.log; \
			false; \
	fi

	$i "Check that no erl_crash.dump file exists"
	$t if ( ! find packages/$1_pkg/ -type f -name erl_crash.dump ); then \
		echo "$(1): erl_crash.dump found" >> packages/errors.log; \
	fi

	$(if $(KEEP_BUILDS),,
		$i "OK; delete the build directory"
		$t rm -rf packages/$1_pkg/)
endef

$(foreach pkg,$(PACKAGES),$(eval $(call pkg_target,$(pkg))))
