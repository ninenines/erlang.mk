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

BUILD_CONFIG_FILE ?= $(CURDIR)/build.config
BUILD_CONFIG = $(shell sed "s/\#.*//" $(BUILD_CONFIG_FILE))

ERLANG_MK = erlang.mk
ERLANG_MK_VERSION = $(shell git describe --tags --dirty)

.PHONY: all

all: pkg
	awk 'FNR==1 && NR!=1{print ""}1' $(patsubst %,%.mk,$(BUILD_CONFIG)) \
	| sed 's/^ERLANG_MK_VERSION = .*/ERLANG_MK_VERSION = $(ERLANG_MK_VERSION)/' > $(ERLANG_MK)

pkg:
	cat packages.v2.tsv | awk 'BEGIN { FS = "\t" }; { print $$1 "\t" $$3 "\t" $$5 "\t" $$6 }' > packages.v1.tsv
	cp packages.v1.tsv packages.v1.txt
