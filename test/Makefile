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

.PHONY: all clean app ct eunit

all: app ct eunit clean
	$i '+---------------------+'
	$i '|  All tests passed.  |'
	$i '+---------------------+'

clean:
	$t rm -rf app1

app: app1
	$i "app: Testing the 'app' target."
	$t make -C app1 app $v
	$i "Checking the modules line in the generated .app file."
	$t [ `grep -E "{modules, *\['m'\]}" app1/ebin/app1.app | wc -l` == 1 ]
	$t [ -e app1/ebin/m.beam ]
	$i "Checking that 'make clean-app' deletes ebin."
	$t make -C app1 clean-app $v
	$t [ ! -e app1/ebin ]
	$i "Checking that 'make app' returns non-zero on compile errors."
	$t printf "%s\n" \
		"-module(syntax_error)." \
		"foo lorem_ipsum dolor sit amet." \
	 > app1/src/syntax_error.erl
	$t if make -C app1 app $v ; then false ; fi
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
	$t make -C app1 ct $v
	$i "Checking files created by 'make ct'."
	$t [ -e app1/test/m_SUITE.beam ]
	$t [ -e app1/ebin/m.beam ]
	$t [ -e app1/logs ]
	$i "Checking that 'make clean' does not delete logs."
	$t make -C app1 clean $v
	$t [ -e app1/logs ]
	$i "Testing target 'ct-mysuite' where mysuite_SUITE is a test suite."
	$t make -C app1 ct-m $v
	$i "Checking that 'make ct' returns non-zero for a failing suite."
	$t printf "%s\n" \
		"-module(failing_SUITE)." \
		"-export([all/0, testcase1/1])." \
		"all() -> [testcase1]." \
		"testcase1(_) -> 42 = m:succ(1)." \
	 > app1/test/failing_SUITE.erl
	$t if make -C app1 ct-failing $v ; then false ; fi
	$i "Checking that 'make distclean-ct' deletes logs."
	$t make -C app1 distclean-ct $v
	$t [ ! -e app1/logs ]
	$t [ -e app1/ebin/m.beam ]
	$i "Cleaning up test data."
	$t rm -rf app1/test
	$i "Test 'ct' passed."

eunit: app1
	$i "eunit: Testing the 'eunit' target."
	$i "Running eunit test case inside module src/t.erl"
	$t printf '%s\n' \
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
	$t make -C app1 eunit $v
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
	$t make -C app1 eunit TEST_DIR=eunit $v
	$i "Checking that 'make eunit' didn't run the tests in t_tests twice, etc."
	$t printf "%s\n" t t_tests x_tests | cmp app1/test-eunit.log -
	$t rm app1/test-eunit.log
	$i "Checking that 'make eunit' returns non-zero for a failing test."
	$t rm -f app1/eunit/*
	$t printf "%s\n" \
		"-module(t_tests)." \
		'-include_lib("eunit/include/eunit.hrl").' \
		"succ_test() ->" \
		"	?assertEqual(42, t:succ(1))." \
		> app1/eunit/t_tests.erl
	$t if make -C app1 eunit TEST_DIR=eunit $v ; then false ; fi
	$t rm -rf app1/eunit app1/src/t.erl app1/test-eunit.log
	$i "Test 'eunit' passed."

# Test application used for testing.
app1:
	$i "Setting up app."
	$t mkdir -p app1
	$t cp ../erlang.mk app1/
	$t make -C app1 -f erlang.mk bootstrap-lib
	$t printf "%s\n" \
		"-module(m)." \
		"-export([succ/1])." \
		"succ(N) -> N + 1." \
		> app1/src/m.erl
