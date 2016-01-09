# AsciiDoc plugin.

ASCIIDOC_CASES = build docs guide install manual
ASCIIDOC_TARGETS = $(addprefix asciidoc-,$(ASCIIDOC_CASES))

.PHONY: asciidoc $(ASCIIDOC_TARGETS)

asciidoc: $(ASCIIDOC_TARGETS)

asciidoc-build: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Only enable man pages section 3"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "MAN_SECTIONS = 3\n"}' $(APP)/Makefile

	$i "Run AsciiDoc"
	$t $(MAKE) -C $(APP) asciidoc $v

	$i "Check that no documentation was generated"
	$t test ! -e $(APP)/doc/guide.pdf
	$t test ! -e $(APP)/doc/html/
	$t test ! -e $(APP)/doc/man3/

	$i "Generate AsciiDoc documentation"
	$t mkdir -p $(APP)/doc/src/guide/ $(APP)/doc/src/manual/
	$t printf "%s\n" \
		"= Erlang.mk tests" "" \
		"Hello world!" > $(APP)/doc/src/guide/book.asciidoc
	$t printf "%s\n" \
		"= erlang_mk(3)" "" \
		"== Name" "" \
		"erlang_mk - Erlang.mk test" "" \
		"== Description" "" \
		"Hello world!" > $(APP)/doc/src/manual/erlang_mk.asciidoc

	$i "Run AsciiDoc"
	$t $(MAKE) -C $(APP) asciidoc $v

	$i "Check that the documentation was generated"
	$t test -f $(APP)/doc/guide.pdf
	$t test -d $(APP)/doc/html/
	$t test -f $(APP)/doc/man3/erlang_mk.3.gz

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that the generated documentation was removed"
	$t test ! -e $(APP)/doc/guide.pdf
	$t test ! -e $(APP)/doc/html/
	$t test ! -e $(APP)/doc/man3/

	$i "Generate an invalid AsciiDoc file"
	$t printf "%s\n" \
		"= fail(3)" "" \
		"This will fail because the Name section is missing." > $(APP)/doc/src/manual/fail.asciidoc

	$i "Check that AsciiDoc errors out"
	$t ! $(MAKE) -C $(APP) asciidoc $v

asciidoc-docs: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate AsciiDoc documentation"
	$t mkdir -p $(APP)/doc/src/guide/
	$t printf "%s\n" \
		"= Erlang.mk tests" "" \
		"Hello world!" > $(APP)/doc/src/guide/book.asciidoc

	$i "Check that AsciiDoc runs on 'make docs'"
	$t $(MAKE) -C $(APP) docs $v
	$t test -f $(APP)/doc/guide.pdf
	$t test -d $(APP)/doc/html/

asciidoc-guide: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate AsciiDoc documentation"
	$t mkdir -p $(APP)/doc/src/guide/ $(APP)/doc/src/manual/
	$t printf "%s\n" \
		"= Erlang.mk tests" "" \
		"Hello world!" > $(APP)/doc/src/guide/book.asciidoc
	$t printf "%s\n" \
		"= erlang_mk(3)" "" \
		"== Name" "" \
		"erlang_mk - Erlang.mk test" "" \
		"== Description" "" \
		"Hello world!" > $(APP)/doc/src/manual/erlang_mk.asciidoc

	$i "Check that only the guide is generated on 'make asciidoc-guide'"
	$t $(MAKE) -C $(APP) asciidoc-guide $v
	$t test -f $(APP)/doc/guide.pdf
	$t test -d $(APP)/doc/html/
	$t test ! -e $(APP)/doc/man3/

asciidoc-install: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Only enable man pages section 3"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "MAN_SECTIONS = 3\n"}' $(APP)/Makefile

	$i "Generate AsciiDoc documentation"
	$t mkdir -p $(APP)/doc/src/manual/
	$t printf "%s\n" \
		"= erlang_mk(3)" "" \
		"== Name" "" \
		"erlang_mk - Erlang.mk test" "" \
		"== Description" "" \
		"Hello world!" > $(APP)/doc/src/manual/erlang_mk.asciidoc

	$i "Build and install the man pages to $(APP)/installed/"
	$t $(MAKE) -C $(APP) install-docs MAN_INSTALL_PATH=installed/share $v

	$i "Check that the documentation was installed properly"
	$t test -f $(APP)/installed/share/man3/erlang_mk.3.gz

asciidoc-manual: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Only enable man pages section 3"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "MAN_SECTIONS = 3\n"}' $(APP)/Makefile

	$i "Generate AsciiDoc documentation"
	$t mkdir -p $(APP)/doc/src/guide/ $(APP)/doc/src/manual/
	$t printf "%s\n" \
		"= Erlang.mk tests" "" \
		"Hello world!" > $(APP)/doc/src/guide/book.asciidoc
	$t printf "%s\n" \
		"= erlang_mk(3)" "" \
		"== Name" "" \
		"erlang_mk - Erlang.mk test" "" \
		"== Description" "" \
		"Hello world!" > $(APP)/doc/src/manual/erlang_mk.asciidoc

	$i "Check that only the manual is generated on 'make asciidoc-manual'"
	$t $(MAKE) -C $(APP) asciidoc-manual $v
	$t test ! -e $(APP)/doc/guide.pdf
	$t test ! -e $(APP)/doc/html/
	$t test -f $(APP)/doc/man3/erlang_mk.3.gz
