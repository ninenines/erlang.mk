# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: asciidoc asciidoc-guide asciidoc-manual install-asciidoc distclean-asciidoc

MAN_INSTALL_PATH ?= /usr/local/share/man
MAN_SECTIONS ?= 3 7

docs:: asciidoc

asciidoc: asciidoc-guide asciidoc-manual

ifeq ($(wildcard doc/src/guide/book.asciidoc),)
asciidoc-guide:
else
asciidoc-guide: distclean-asciidoc doc-deps
	a2x -v -f pdf doc/src/guide/book.asciidoc && mv doc/src/guide/book.pdf doc/guide.pdf
	a2x -v -f chunked doc/src/guide/book.asciidoc && mv doc/src/guide/book.chunked/ doc/html/
endif

ifeq ($(wildcard doc/src/manual/*.asciidoc),)
asciidoc-manual:
else
asciidoc-manual: distclean-asciidoc doc-deps
	for f in doc/src/manual/*.asciidoc ; do \
		a2x -v -f manpage $$f ; \
	done
	for s in $(MAN_SECTIONS); do \
		mkdir -p doc/man$$s/ ; \
		mv doc/src/manual/*.$$s doc/man$$s/ ; \
		gzip doc/man$$s/*.$$s ; \
	done

install-docs:: install-asciidoc

install-asciidoc: asciidoc-manual
	for s in $(MAN_SECTIONS); do \
		mkdir -p $(MAN_INSTALL_PATH)/man$$s/ ; \
		install -g `id -u` -o `id -g` -m 0644 doc/man$$s/*.gz $(MAN_INSTALL_PATH)/man$$s/ ; \
	done
endif

distclean:: distclean-asciidoc

distclean-asciidoc:
	$(gen_verbose) rm -rf doc/html/ doc/guide.pdf doc/man3/ doc/man7/
