# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
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

WITHOUT ?=

BUILD_CONFIG_FILE ?= $(CURDIR)/build.config

ifeq ($(strip $(WITHOUT)),)
BUILD_CONFIG = $(shell sed "s/\#.*//" $(BUILD_CONFIG_FILE))
else
empty := $(subst ,, )
BUILD_CONFIG = $(shell sed "s/\#.*//" $(BUILD_CONFIG_FILE) \
	| grep -v "^$(subst $(empty),\|^,$(WITHOUT))")
endif

ERLANG_MK ?= erlang.mk
ERLANG_MK_VERSION = $(shell git describe --dirty --tags --always)

.PHONY: all check

all:
	export LC_COLLATE=C; \
	awk 'FNR==1 && NR!=1{print ""}1' $(patsubst %,%.mk,$(BUILD_CONFIG)) \
		| sed 's/^ERLANG_MK_VERSION =.*/ERLANG_MK_VERSION = $(ERLANG_MK_VERSION)/' \
		| sed 's:^ERLANG_MK_WITHOUT =.*:ERLANG_MK_WITHOUT = $(WITHOUT):' > $(ERLANG_MK)

lint: all
	$(MAKE) -f erlang.mk --warn-undefined-variables

ifdef p
# Remove p from the list of variables since that conflicts with bootstrapping.
MAKEOVERRIDES := $(filter-out p=$p,$(MAKEOVERRIDES))

check:
	$(MAKE) -C test pkg-$p KEEP_BUILDS=1
else
ifdef c
check:
	$(MAKE) -C test $c c=
else
check:
	$(MAKE) -C test
endif
endif

packages:
	$(MAKE) -C test packages

summary:
	@mkdir -p test/logs/
	@touch test/logs/latest.log test/packages/errors.log
	-@sort test/packages/errors.log | diff test/logs/latest.log -
	@sort test/packages/errors.log > test/logs/latest.log
	@cp test/logs/latest.log "test/logs/$(shell date '+%F_%T%z')"

search:
	@$(MAKE) --no-print-directory \
		-f core/core.mk $(addprefix -f,$(wildcard index/*.mk)) -f core/index.mk \
		search

clean:
	$(MAKE) -C test clean
	rm -rf doc/guide.pdf doc/html
	git checkout erlang.mk

docs:
	$(MAKE) -f core/core.mk -f core/docs.mk -f plugins/asciidoc.mk asciidoc DEPS=asciideck

up:
	git clone git@github.com:ninenines/erlang.mk.git gh-pages
	cd gh-pages && git checkout gh-pages
	cd gh-pages && make
	cd gh-pages && git push origin gh-pages
	rm -rf gh-pages

ifdef file
up-relx:
	git clone git@github.com:ninenines/erlang.mk.git gh-pages
	cd gh-pages && git checkout gh-pages
	cp $(file) gh-pages/res/
	cd gh-pages && git add .
	cd gh-pages && git commit -m "Publish new Relx version"
	cd gh-pages && git push origin gh-pages
	rm -rf gh-pages
else
up-relx:
	@echo Missing parameter: url
endif

ifdef p
XDG_DATA_HOME ?= $(HOME)/.local/share
INSTALL_DIR ?= $(XDG_DATA_HOME)/erlang.mk/lib

install: check
	for dep in `ls test/packages/$p_pkg/deps/`; do \
		rm -rf $(INSTALL_DIR)/$$dep; \
		mkdir -p $(INSTALL_DIR)/$$dep; \
		cp -r test/packages/$p_pkg/deps/$$dep/ebin $(INSTALL_DIR)/$$dep/; \
		if [ -d test/packages/$p_pkg/deps/$$dep/priv ]; then \
			cp -r test/packages/$p_pkg/deps/$$dep/priv $(INSTALL_DIR)/$$dep/; \
		fi; \
	done
else
install:
	@echo "Usage: $(MAKE) install p=<package name>"
endif

ifneq ($(strip $(LATEST_ERLANG_OTP)),)
include core/kerl.mk
endif
