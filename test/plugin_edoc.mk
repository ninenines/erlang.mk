# EDoc plugin.

EDOC_CASES = build docs no-overview opts src-dirs
EDOC_TARGETS = $(addprefix edoc-,$(EDOC_CASES))

.PHONY: edoc $(EDOC_TARGETS)

edoc: $(EDOC_TARGETS)

edoc-build: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Run EDoc"
	$t $(MAKE) -C $(APP) edoc $v

	$i "Check that documentation was generated"
	$t test -f $(APP)/doc/index.html
	$t test -f $(APP)/doc/$(APP)_app.html
	$t test -f $(APP)/doc/$(APP)_sup.html

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that the generated documentation was removed"
	$t test ! -e $(APP)/doc/index.html
	$t test ! -e $(APP)/doc/$(APP)_app.html
	$t test ! -e $(APP)/doc/$(APP)_sup.html

	$i "Generate a module with EDoc comments"
	$t printf "%s\n" \
		"%% @doc erlang-mk-edoc-module" \
		"-module($(APP))." \
		"-export([ok/0])." \
		"" \
		"%% @doc erlang-mk-edoc-function" \
		"ok() -> ok." > $(APP)/src/$(APP).erl

	$i "Run EDoc"
	$t $(MAKE) -C $(APP) edoc $v

	$i "Check that the new module's documentation was generated"
	$t test -f $(APP)/doc/$(APP).html

	$i "Check that the EDoc comments are in the generated documentation"
	$t grep -q erlang-mk-edoc-module $(APP)/doc/$(APP).html
	$t grep -q erlang-mk-edoc-function $(APP)/doc/$(APP).html

	$i "Generate a module with an invalid EDoc comment"
	$t printf "%s\n" \
		"-module($(APP)_fail)." \
		"-export([fail/0])." \
		"%% @spec lol" \
		"fail() -> fail." > $(APP)/src/$(APP)_fail.erl

	$i "Check that EDoc errors out"
	$t ! $(MAKE) -C $(APP) edoc $v

edoc-docs: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a doc/overview.edoc file"
	$t mkdir $(APP)/doc
	$t printf "%s\n" \
		"@author R. J. Hacker <rjh@acme.com>" \
		"@copyright 2007 R. J. Hacker" \
		"@version 1.0.0" \
		"@title Welcome to the 'frob' application!" \
		"@doc 'frob' is a highly advanced frobnicator with low latency," > $(APP)/doc/overview.edoc

	$i "Check that EDoc runs on 'make docs'"
	$t $(MAKE) -C $(APP) docs $v
	$t test -f $(APP)/doc/index.html

	$i "Check that the overview.edoc file was used"
	$t grep -q frobnicator $(APP)/doc/overview-summary.html

edoc-no-overview: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Check that EDoc doesn't run on 'make docs'"
	$t $(MAKE) -C $(APP) docs $v
	$t test ! -e $(APP)/doc/index.html

edoc-opts: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add edown doclet for EDoc"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DOC_DEPS = edown\nEDOC_OPTS = {doclet, edown_doclet}\n"}' $(APP)/Makefile

	$i "Run EDoc"
	$t $(MAKE) -C $(APP) edoc $v

	$i "Check that the Markdown documentation was generated"
	$t test -f $(APP)/doc/README.md
	$t test -f $(APP)/doc/$(APP)_app.md
	$t test -f $(APP)/doc/$(APP)_sup.md

edoc-src-dirs: build clean

	$i "Create a multi application repository with a root application"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a new application my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Add apps directories to the list of EDoc source directories"
	$t echo 'EDOC_SRC_DIRS = $$(ALL_APPS_DIRS)' >> $(APP)/Makefile

	$i "Run EDoc"
	$t $(MAKE) -C $(APP) edoc $v

	$i "Check that the generated documentation includes modules from both apps"
	$t test -f $(APP)/doc/index.html
	$t test -f $(APP)/doc/$(APP)_app.html
	$t test -f $(APP)/doc/$(APP)_sup.html
	$t test -f $(APP)/doc/my_app_app.html
	$t test -f $(APP)/doc/my_app_sup.html
