# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
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

.PHONY: all deps app rel docs tests clean distclean help

ERLANG_MK_VERSION = 1

# Core configuration.

PROJECT ?= $(notdir $(CURDIR))

# Verbosity.

V ?= 0

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

# Core targets.

all:: deps app rel

clean::
	$(gen_verbose) rm -f erl_crash.dump

distclean:: clean

help::
	@printf "%s\n" \
		"erlang.mk (version $(ERLANG_MK_VERSION)) is distributed under the terms of the ISC License." \
		"Copyright (c) 2013-2014 Loïc Hoguin <essen@ninenines.eu>" \
		"" \
		"Usage: [V=1] make [target]" \
		"" \
		"Core targets:" \
		"  all         Run deps, app and rel targets in that order" \
		"  deps        Fetch dependencies (if needed) and compile them" \
		"  app         Compile the project" \
		"  rel         Build a release for this project, if applicable" \
		"  docs        Build the documentation for this project" \
		"  tests       Run the tests for this project" \
		"  clean       Delete temporary and output files from most targets" \
		"  distclean   Delete all temporary and output files" \
		"  help        Display this help and exit" \
		"" \
		"The target clean only removes files that are commonly removed." \
		"Dependencies and releases are left untouched." \
		"" \
		"Setting V=1 when calling make enables verbose mode."

# Core functions.

define core_http_get
	wget --no-check-certificate -O $(1) $(2)|| rm $(1)
endef
