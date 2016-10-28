# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: sfx

ifdef RELX_REL
ifdef SFX

# Configuration.

SFX_ARCHIVE ?= $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/$(RELX_REL_NAME)-$(RELX_REL_VSN).tar.gz
SFX_OUTPUT_FILE ?= $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME).run

# Core targets.

rel:: sfx

# Plugin-specific targets.

define sfx_stub
#!/bin/sh

TMPDIR=`mktemp -d`
ARCHIVE=`awk '/^__ARCHIVE_BELOW__$$/ {print NR + 1; exit 0;}' $$0`
FILENAME=$$(basename $$0)
REL=$${FILENAME%.*}

tail -n+$$ARCHIVE $$0 | tar -xzf - -C $$TMPDIR

$$TMPDIR/bin/$$REL console
RET=$$?

rm -rf $$TMPDIR

exit $$RET

__ARCHIVE_BELOW__
endef

sfx:
	$(call render_template,sfx_stub,$(SFX_OUTPUT_FILE))
	$(gen_verbose) cat $(SFX_ARCHIVE) >> $(SFX_OUTPUT_FILE)
	$(verbose) chmod +x $(SFX_OUTPUT_FILE)

endif
endif
