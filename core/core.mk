# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

.PHONY: all deps app rel docs install-docs tests check clean distclean help erlang-mk

ERLANG_MK_VERSION = 1

# Core configuration.

PROJECT ?= $(notdir $(CURDIR))
PROJECT := $(strip $(PROJECT))

# Verbosity.

V ?= 0

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

# "erl" command.

ERL = erl +A0 -noinput -boot start_clean

# Platform detection.
# @todo Add Windows/Cygwin detection eventually.

ifeq ($(PLATFORM),)
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
PLATFORM = linux
else ifeq ($(UNAME_S),Darwin)
PLATFORM = darwin
else ifeq ($(UNAME_S),SunOS)
PLATFORM = solaris
else ifeq ($(UNAME_S),GNU)
PLATFORM = gnu
else ifeq ($(UNAME_S),FreeBSD)
PLATFORM = freebsd
else ifeq ($(UNAME_S),NetBSD)
PLATFORM = netbsd
else ifeq ($(UNAME_S),OpenBSD)
PLATFORM = openbsd
else
$(error Unable to detect platform. Please open a ticket with the output of uname -a.)
endif

export PLATFORM
endif

# Core targets.

ifneq ($(words $(MAKECMDGOALS)),1)
.NOTPARALLEL:
endif

all:: deps
	@$(MAKE) --no-print-directory app
	@$(MAKE) --no-print-directory rel

# Noop to avoid a Make warning when there's nothing to do.
rel::
	@echo -n

check:: clean app tests

clean:: clean-crashdump

clean-crashdump:
ifneq ($(wildcard erl_crash.dump),)
	$(gen_verbose) rm -f erl_crash.dump
endif

distclean:: clean

help::
	@printf "%s\n" \
		"erlang.mk (version $(ERLANG_MK_VERSION)) is distributed under the terms of the ISC License." \
		"Copyright (c) 2013-2014 Loïc Hoguin <essen@ninenines.eu>" \
		"" \
		"Usage: [V=1] $(MAKE) [-jNUM] [target]" \
		"" \
		"Core targets:" \
		"  all           Run deps, app and rel targets in that order" \
		"  deps          Fetch dependencies (if needed) and compile them" \
		"  app           Compile the project" \
		"  rel           Build a release for this project, if applicable" \
		"  docs          Build the documentation for this project" \
		"  install-docs  Install the man pages for this project" \
		"  tests         Run the tests for this project" \
		"  check         Compile and run all tests and analysis for this project" \
		"  clean         Delete temporary and output files from most targets" \
		"  distclean     Delete all temporary and output files" \
		"  help          Display this help and exit" \
		"" \
		"The target clean only removes files that are commonly removed." \
		"Dependencies and releases are left untouched." \
		"" \
		"Setting V=1 when calling $(MAKE) enables verbose mode." \
		"Parallel execution is supported through the -j $(MAKE) flag."

# Core functions.

define newline


endef

define erlang
$(ERL) -eval "$(subst $(newline),,$(subst ",\",$(1)))"
endef

ifeq ($(shell which wget 2>/dev/null | wc -l), 1)
define core_http_get
	wget --no-check-certificate -O $(1) $(2)|| rm $(1)
endef
else
define core_http_get.erl
	ssl:start(),
	inets:start(),
	case httpc:request(get, {"$(2)", []}, [{autoredirect, true}], []) of
		{ok, {{_, 200, _}, _, Body}} ->
			case file:write_file("$(1)", Body) of
				ok -> ok;
				{error, R1} -> halt(R1)
			end;
		{error, R2} ->
			halt(R2)
	end,
	halt(0).
endef

define core_http_get
	$(call erlang,$(call core_http_get.erl,$(1),$(2)))
endef
endif

# Automated update.

ERLANG_MK_BUILD_CONFIG ?= build.config
ERLANG_MK_BUILD_DIR ?= .erlang.mk.build

erlang-mk:
	git clone https://github.com/ninenines/erlang.mk $(ERLANG_MK_BUILD_DIR)
	if [ -f $(ERLANG_MK_BUILD_CONFIG) ]; then cp $(ERLANG_MK_BUILD_CONFIG) $(ERLANG_MK_BUILD_DIR); fi
	cd $(ERLANG_MK_BUILD_DIR) && $(MAKE)
	cp $(ERLANG_MK_BUILD_DIR)/erlang.mk ./erlang.mk
	rm -rf $(ERLANG_MK_BUILD_DIR)
