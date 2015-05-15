# Copyright (c) 2014, Viktor Söderqvist <viktor@zuiderkwast.se>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Tests for erlang.mk targets. If any test fails or if you run a target other
# than 'all', you must probably do 'make clean' before you can test again.

# Verbosity.

V ?= 0

# t = Verbosity control for tests
# v = Verbosity control for erlang.mk
# i = Command to display (or suppress) info messages
ifeq ($V,0)
	# Show info messages only
	t = @
	v = V=0 &>/dev/null
	i = @echo
else ifeq ($V,1)
	# Show test commands
	t =
	v = V=0 &>/dev/null
	i = @echo ==
else ifeq ($V,2)
	# Show briefly what erlang.mk is doing
	t = @echo " TEST  " $@;
	v = V=0
	i = @echo ==
else
	# Show all commands with maximum verbosity
	t =
	v = V=1
	i = @echo ==
endif

.PHONY: all clean app ct eunit tests-cover docs

.NOTPARALLEL:

all: clean app ct eunit tests-cover docs pkgs
	$i '+---------------------+'
	$i '|  All tests passed.  |'
	$i '+---------------------+'

clean:
	$t rm -rf app1

app: app1
	$i "app: Testing the 'app' target."
	$t $(MAKE) -C app1 app $v
	$i "Checking the modules line in the generated .app file."
	$t [ `grep -E "{modules, *\['m'\]}" app1/ebin/app1.app | wc -l` == 1 ]
	$t [ -e app1/ebin/m.beam ]
	$i "Checking that '$(MAKE) clean-app' deletes ebin."
	$t $(MAKE) -C app1 clean-app $v
	$t [ ! -e app1/ebin ]
	$i "Checking that '$(MAKE) app' returns non-zero on compile errors."
	$t printf "%s\n" \
		"-module(syntax_error)." \
		"foo lorem_ipsum dolor sit amet." \
	 > app1/src/syntax_error.erl
	$t if $(MAKE) -C app1 app $v ; then false ; fi
	$t rm app1/src/syntax_error.erl
	$i "Test 'app' passed."

ct: app1
	$i "ct: Testing ct and related targets."
	$i "Setting up test suite."
	$t mkdir -p app1/test
	$t printf "%s\n" \
		"-module(m_SUITE)." \
		"-export([all/0, testcase1/1])." \
		"all() -> [testcase1]." \
		"testcase1(_) -> 2 = m:succ(1)." \
	 > app1/test/m_SUITE.erl
	$t $(MAKE) -C app1 ct $v
	$i "Checking files created by '$(MAKE) ct'."
	$t [ -e app1/test/m_SUITE.beam ]
	$t [ -e app1/ebin/m.beam ]
	$t [ -e app1/logs ]
	$i "Checking that '$(MAKE) clean' does not delete logs."
	$t $(MAKE) -C app1 clean $v
	$t [ -e app1/logs ]
	$i "Testing target 'ct-mysuite' where mysuite_SUITE is a test suite."
	$t $(MAKE) -C app1 ct-m $v
	$i "Checking that '$(MAKE) ct' returns non-zero for a failing suite."
	$t printf "%s\n" \
		"-module(failing_SUITE)." \
		"-export([all/0, testcase1/1])." \
		"all() -> [testcase1]." \
		"testcase1(_) -> 42 = m:succ(1)." \
	 > app1/test/failing_SUITE.erl
	$t if $(MAKE) -C app1 ct-failing $v ; then false ; fi
	$i "Checking that '$(MAKE) distclean-ct' deletes logs."
	$t $(MAKE) -C app1 distclean-ct $v
	$t [ ! -e app1/logs ]
	$t [ -e app1/ebin/m.beam ]
	$i "Cleaning up test data."
	$t rm -rf app1/test
	$i "Test 'ct' passed."

eunit: app1
	$i "eunit: Testing the 'eunit' target."
	$i "Running eunit test case inside module src/t.erl"
	$t $(call create-module-t)
	$t $(MAKE) -C app1 eunit $v
	$i "Checking that the eunit test in module t."
	$t echo t | cmp app1/test-eunit.log -
	$t rm app1/test-eunit.log
	$i "Running eunit tests in a separate directory."
	$t mkdir -p app1/eunit
	$t printf '%s\n' \
		'-module(t_tests).' \
		'-include_lib("eunit/include/eunit.hrl").' \
		'succ_test() ->' \
		'	?assertEqual(2, t:succ(1)),' \
		'	os:cmd("echo t_tests >> test-eunit.log").' \
		> app1/eunit/t_tests.erl
	$t printf '%s\n' \
		'-module(x_tests).' \
		'-include_lib("eunit/include/eunit.hrl").' \
		'succ_test() ->' \
		'	?assertEqual(2, t:succ(1)),' \
		'	os:cmd("echo x_tests >> test-eunit.log").' \
		> app1/eunit/x_tests.erl
	$t $(MAKE) -C app1 eunit TEST_DIR=eunit $v
	$i "Checking that '$(MAKE) eunit' didn't run the tests in t_tests twice, etc."
	$t printf "%s\n" t t_tests x_tests | cmp app1/test-eunit.log -
	$t rm app1/test-eunit.log
	$i "Checking that '$(MAKE) eunit' returns non-zero for a failing test."
	$t rm -f app1/eunit/*
	$t printf "%s\n" \
		"-module(t_tests)." \
		'-include_lib("eunit/include/eunit.hrl").' \
		"succ_test() ->" \
		"	?assertEqual(42, t:succ(1))." \
		> app1/eunit/t_tests.erl
	$t if $(MAKE) -C app1 eunit TEST_DIR=eunit $v ; then false ; fi
	$t rm -rf app1/eunit app1/src/t.erl app1/test-eunit.log
	$i "Test 'eunit' passed."

# TODO: do coverage for 'tests' instead of 'eunit ct' when triq is fixed
tests-cover: app1
	$i "tests-cover: Testing 'eunit' and 'ct' with COVER=1"
	$i "Setting up eunit and ct suites."
	$t $(call create-module-t)
	$t mkdir -p app1/test
	$t printf "%s\n" \
		"-module(m_SUITE)." \
		"-export([all/0, testcase1/1])." \
		"all() -> [testcase1]." \
		"testcase1(_) -> 2 = m:succ(1)." \
	 > app1/test/m_SUITE.erl
	$i "Running tests with coverage analysis."
	$t $(MAKE) -C app1 eunit ct COVER=1 $v
	$t [ -e app1/test-eunit.log ]
	$t [ -e app1/eunit.coverdata ]
	$t [ -e app1/ct.coverdata ]
	$i "Generating coverage report."
	$t $(MAKE) -C app1 cover-report COVER=1 $v
	$t [ -e app1/cover/m.COVER.html ]
	$t [ -e app1/cover/t.COVER.html ]
	$t [ -e app1/cover/index.html ]
	$i "Checking combined coverage from eunit and ct."
	$t [ `grep 'Total: 100%' app1/cover/index.html | wc -l` -eq 1 ]
	$i "Checking that cover-report-clean removes cover report."
	$t $(MAKE) -C app1 cover-report-clean $v
	$t [ ! -e app1/cover ]
	$i "Checking that coverdata-clean removes cover data."
	$t $(MAKE) -C app1 coverdata-clean $v
	$t [ ! -e app1/eunit.coverdata ]
	@# clean up
	$t rm -rf app1/src/t.erl app1/test app1/test-eunit.log
	$t $(MAKE) -C app1 clean $v
	$i "Test 'tests-cover' passed."

docs: app1
	$i "docs: Testing EDoc including DOC_DEPS."
	$t printf "%s\n" \
		"PROJECT = app1" \
		"DOC_DEPS = edown" \
		"dep_edown = git https://github.com/uwiger/edown.git 0.5" \
		"EDOC_OPTS = {doclet, edown_doclet}" \
		"include erlang.mk" \
		"distclean:: distclean-doc-md" \
		"distclean-doc-md:" \
		"	rm -rf doc/*.md" \
		> app1/Makefile-doc
	$i "Downloading doc deps (edown) and building docs."
	$t $(MAKE) -C app1 -f Makefile-doc docs $v
	$i "Checking that '$(MAKE) docs' using edown generated a markdown file."
	$t [ -e app1/doc/m.md ]
	$i "Checking that '$(MAKE) distclean' deletes all generated doc files."
	$t $(MAKE) -C app1 -f Makefile-doc distclean $v
	$t [ "`ls app1/doc/`" == "" ]
	$i "Cleaning up test data."
	$t rm app1/Makefile-doc
	$i "Test 'docs' passed."

define app1_setup
	$i "Setting up app."
	$t mkdir -p app1
	$t cd .. && $(MAKE)
	$t cp ../erlang.mk app1/
	$t $(MAKE) -C app1 -f erlang.mk bootstrap-lib
	$t printf "%s\n" \
		"-module(m)." \
		"-export([succ/1])." \
		"succ(N) -> N + 1." \
		> app1/src/m.erl
endef

define pkg_test_target
pkg-$(1)-clean:
	$t rm -rf app1

pkg-$(1)-app1:
	$(call app1_setup)

# Running 'make' twice to make sure it recompiles fine.
pkg-$(1): pkg-$(1)-clean pkg-$(1)-app1
	$i
	$i "  pkgs: Checking that '$(1)' builds correctly"
	$i
	$t printf "%s\n" \
		"PROJECT = app1" \
		"DEPS = $(1)" \
		"include erlang.mk" \
		> app1/Makefile
	cp ../packages.v2.tsv app1/.erlang.mk.packages.v2
	$t $(MAKE) -C app1
	$t $(MAKE) -C app1
endef

$(foreach pkg,$(shell awk '{print $$1}' ../packages.v2.tsv),$(eval $(call pkg_test_target,$(pkg))))

pkgs: $(foreach pkg,$(shell awk '{print $$1}' ../packages.v2.tsv),pkg-$(pkg))

# Test application used for testing.
app1:
	$(call app1_setup)

# Extra module in app1 used for testing eunit
define create-module-t
printf '%s\n' \
	'-module(t).' \
	'-export([succ/1]).' \
	'succ(N) -> N + 1.' \
	'-ifdef(TEST).' \
	'-include_lib("eunit/include/eunit.hrl").' \
	'succ_test() ->' \
	'	?assertEqual(2, succ(1)),' \
	'	os:cmd("echo t >> test-eunit.log").' \
	'-endif.' \
	> app1/src/t.erl
endef
