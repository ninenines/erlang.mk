# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifeq ($(filter asciideck,$(DEPS) $(DOC_DEPS)),asciideck)

.PHONY: asciidoc asciidoc-guide asciidoc-manual install-asciidoc distclean-asciidoc-guide distclean-asciidoc-manual

# Core targets.

docs:: asciidoc

distclean:: distclean-asciidoc-guide distclean-asciidoc-manual

# Plugin-specific targets.

asciidoc: asciidoc-guide asciidoc-manual

# User guide.

ifeq ($(wildcard doc/src/guide/book.asciidoc),)
asciidoc-guide:
else
asciidoc-guide: distclean-asciidoc-guide doc-deps
	a2x -v -f pdf doc/src/guide/book.asciidoc && mv doc/src/guide/book.pdf doc/guide.pdf
	a2x -v -f chunked doc/src/guide/book.asciidoc && mv doc/src/guide/book.chunked/ doc/html/

distclean-asciidoc-guide:
	$(gen_verbose) rm -rf doc/html/ doc/guide.pdf
endif

# Man pages.

ASCIIDOC_MANUAL_FILES := $(wildcard doc/src/manual/*.asciidoc)

ifeq ($(ASCIIDOC_MANUAL_FILES),)
asciidoc-manual:
else

# Configuration.

MAN_INSTALL_PATH ?= /usr/local/share/man
MAN_SECTIONS ?= 3 7
MAN_PROJECT ?= $(shell echo $(PROJECT) | sed 's/^./\U&\E/')
MAN_VERSION ?= $(PROJECT_VERSION)

# Plugin-specific targets.

define asciidoc2man.erl
try
	[begin
		io:format(" ADOC   ~s~n", [F]),
		ok = asciideck:to_manpage(asciideck:parse_file(F), #{
			compress => gzip,
			outdir => filename:dirname(F),
			extra2 => "$(MAN_PROJECT) $(MAN_VERSION)",
			extra3 => "$(MAN_PROJECT) Function Reference"
		})
	end || F <- [$(shell echo $(addprefix $(comma)\",$(addsuffix \",$1)) | sed 's/^.//')]],
	halt(0)
catch C:E ->
	io:format("Exception ~p:~p~nStacktrace: ~p~n", [C, E, erlang:get_stacktrace()]),
	halt(1)
end.
endef

asciidoc-manual:: doc-deps

asciidoc-manual:: $(ASCIIDOC_MANUAL_FILES)
	$(call erlang,$(call asciidoc2man.erl,$?))
	$(foreach s,$(MAN_SECTIONS),mkdir -p doc/man$s/ && mv doc/src/manual/*.$s.gz doc/man$s/;)

install-docs:: install-asciidoc

install-asciidoc: asciidoc-manual
	$(foreach s,$(MAN_SECTIONS),\
		mkdir -p $(MAN_INSTALL_PATH)/man$s/ && \
		install -g `id -g` -o `id -u` -m 0644 doc/man$s/*.gz $(MAN_INSTALL_PATH)/man$s/;)

distclean-asciidoc-manual:
	$(gen_verbose) rm -rf $(addprefix doc/man,$(MAN_SECTIONS))
endif
endif
