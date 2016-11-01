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

	$i "Add asciideck to the local dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DOC_DEPS = asciideck\n"}' $(APP)/Makefile

	$i "Run AsciiDoc"
	$t $(MAKE) -C $(APP) asciidoc $v

	$i "Check that no documentation was generated"
	$t test ! -e $(APP)/doc/guide.pdf
	$t test ! -e $(APP)/doc/html/
	$t test ! -e $(APP)/doc/man3/
	$t test ! -e $(APP)/doc/man7/

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
	$t printf "%s\n" \
		"= erlang_mk(7)" "" \
		"== Name" "" \
		"erlang_mk - Erlang.mk application" "" \
		"== Description" "" \
		"Summer is better than winter!" > $(APP)/doc/src/manual/erlang_mk_app.asciidoc

	$i "Run AsciiDoc"
	$t $(MAKE) -C $(APP) asciidoc $v

	$i "Check that the documentation was generated"
	$t test -f $(APP)/doc/guide.pdf
	$t test -d $(APP)/doc/html/
	$t test -f $(APP)/doc/man3/erlang_mk.3.gz
	$t test -f $(APP)/doc/man7/erlang_mk.7.gz

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that the generated documentation was removed"
	$t test ! -e $(APP)/doc/guide.pdf
	$t test ! -e $(APP)/doc/html/
	$t test ! -e $(APP)/doc/man3/
	$t test ! -e $(APP)/doc/man7/

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

	$i "Add asciideck to the local dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DOC_DEPS = asciideck\n"}' $(APP)/Makefile

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

	$i "Add asciideck to the local dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DOC_DEPS = asciideck\n"}' $(APP)/Makefile

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

	$i "Add asciideck to the local dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DOC_DEPS = asciideck\n"}' $(APP)/Makefile

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

	$i "Add asciideck to the local dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DOC_DEPS = asciideck\n"}' $(APP)/Makefile

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
	$t printf "%s\n" \
		"= name_changed(3)" "" \
		"== Name" "" \
		"name_changed - Manual page name different than output" "" \
		"== Description" "" \
		"Name changed!" > $(APP)/doc/src/manual/change_name.asciidoc

	$i "Run 'make asciidoc-manual'"
	$t $(MAKE) -C $(APP) asciidoc-manual $v

	$i "Check that only the manual was generated"
	$t test ! -e $(APP)/doc/guide.pdf
	$t test ! -e $(APP)/doc/html/
	$t test -f $(APP)/doc/man3/erlang_mk.3.gz
	$t test -f $(APP)/doc/man3/name_changed.3.gz
