# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: sfx

ifdef RELX_RELEASE
ifdef SFX

# Configuration.

SFX_OUTPUT = $(RELX_OUTPUT_DIR)/$(RELX_RELEASE).run

# Core targets.

rel:: sfx

# Plugin-specific targets.

define sfx_stub
#!/bin/sh

TMPDIR=`mktemp -d`
ARCHIVE=`awk '/^__ARCHIVE_BELOW__$$/ {print NR + 1; exit 0;}' $$0`
FILENAME=$$(basename $$0)
REL=$${FILENAME%.*}

tail -n+$$ARCHIVE $$0 | tar xz -C $$TMPDIR

$$TMPDIR/$$REL/bin/$$REL console
RET=$$?

rm -rf $$TMPDIR

exit $$RET

__ARCHIVE_BELOW__
endef

sfx:
	$(call render_template,sfx_stub,$(SFX_OUTPUT))
	$(gen_verbose) tar -C $(RELX_OUTPUT_DIR) -czf $(RELX_OUTPUT_DIR)/$(RELX_RELEASE).tgz $(RELX_RELEASE)
	$(verbose) cat $(RELX_OUTPUT_DIR)/$(RELX_RELEASE).tgz >> $(SFX_OUTPUT)
	$(verbose) chmod +x $(SFX_OUTPUT)
	$(verbose) rm $(RELX_OUTPUT_DIR)/$(RELX_RELEASE).tgz

endif
endif
