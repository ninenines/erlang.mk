# Copyright (c) 2014, Viktor Söderqvist <viktor@zuiderkwast.se>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Tests for erlang.mk targets. If any test fails or if you run a target other
# than 'all', you must probably do 'make clean' before you can test again.

# Verbosity.

V ?= 0

# Temporary files directory.

ERLANG_MK_TMP=$(CURDIR)/tmp
export ERLANG_MK_TMP

# Temporary application name, taken from rule name.

APP = $(subst -,_,$@)
APP_TO_CLEAN = $(subst -,_,$(patsubst clean-%,%,$@))

# Erlang, quickly!

ERL = erl +A0 -noinput -boot start_clean

# t = Verbosity control for tests
# v = Verbosity control for erlang.mk
# i = Command to display (or suppress) info messages
ifeq ($V,0)
	# Show info messages only
	t = @
	v = V=0 >/dev/null 2>&1
	i = @echo $@:
else ifeq ($V,1)
	# Show test commands
	t =
	v = V=0 >/dev/null 2>&1
	i = @echo == $@:
else ifeq ($V,2)
	# Show briefly what erlang.mk is doing
	t = @echo " TEST  " $@;
	v = V=0
	i = @echo == $@:
else
	# Show all commands with maximum verbosity
	t =
	v = V=1
	i = @echo == $@:
endif

.PHONY: all clean app ct eunit tests-cover docs

.NOTPARALLEL:

all: clean bootstrap app ct eunit tests-cover docs pkgs
	$i '+---------------------+'
	$i '|  All tests passed.  |'
	$i '+---------------------+'

clean: clean-bootstrap
	$t rm -rf app1 pkgs.log $(ERLANG_MK_TMP)

build:
	$i "Generate a bleeding edge Erlang.mk"
	$t cd .. && $(MAKE) $v

# Bootstrap plugin.

BOOTSTRAP_CASES = app lib rel templates
BOOTSTRAP_TARGETS = $(addprefix bootstrap-,$(BOOTSTRAP_CASES))
BOOTSTRAP_CLEAN_TARGETS = $(addprefix clean-,$(BOOTSTRAP_TARGETS))

.PHONY: bootstrap $(BOOTSTRAP_TARGETS) $(BOOTSTRAP_CLEAN_TARGETS)

bootstrap: $(BOOTSTRAP_TARGETS)

bootstrap-app: build clean-bootstrap-app

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/Makefile
	$t test -f $(APP)/src/$(APP).app.src
	$t test -f $(APP)/src/$(APP)_app.erl
	$t test -f $(APP)/src/$(APP)_sup.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP)_app.beam
	$t test -f $(APP)/ebin/$(APP)_sup.beam

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, [$(APP)_app, $(APP)_sup]} = application:get_key($(APP), modules), \
		{module, $(APP)_app} = code:load_file($(APP)_app), \
		{module, $(APP)_sup} = code:load_file($(APP)_sup), \
		halt()"

bootstrap-lib: build clean-bootstrap-lib

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/Makefile
	$t test -f $(APP)/src/$(APP).app.src

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, []} = application:get_key($(APP), modules), \
		halt()"

bootstrap-rel: build clean-bootstrap-rel

	$i "Bootstrap a new release-enabled OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/Makefile
	$t test -f $(APP)/relx.config
	$t test -f $(APP)/rel/sys.config
	$t test -f $(APP)/rel/vm.args
	$t test -f $(APP)/src/$(APP).app.src
	$t test -f $(APP)/src/$(APP)_app.erl
	$t test -f $(APP)/src/$(APP)_sup.erl

	$i "Build the application and the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP)_app.beam
	$t test -f $(APP)/ebin/$(APP)_sup.beam

	$i "Check that the release was generated"
	$t test -f $(APP)/_rel/$(APP)_release/bin/$(APP)_release

	$i "Check that the release can be started and stopped"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release start $v
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release stop $v

	$i "Check that there's no erl_crash.dump file"
	$t test ! -f $(APP)/_rel/$(APP)_release/erl_crash.dump

bootstrap-templates: build clean-bootstrap-templates

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Check that we can get the list of templates"
	$t test `$(MAKE) -C $(APP) --no-print-directory list-templates V=0 | wc -l` -eq 1

	$i "Generate one of each template"
	$t $(MAKE) -C $(APP) --no-print-directory new t=gen_fsm n=my_fsm
	$t $(MAKE) -C $(APP) --no-print-directory new t=gen_server n=my_server
	$t $(MAKE) -C $(APP) --no-print-directory new t=supervisor n=my_sup
	$t $(MAKE) -C $(APP) --no-print-directory new t=cowboy_http n=my_http
	$t $(MAKE) -C $(APP) --no-print-directory new t=cowboy_loop n=my_loop
	$t $(MAKE) -C $(APP) --no-print-directory new t=cowboy_rest n=my_rest
	$t $(MAKE) -C $(APP) --no-print-directory new t=cowboy_ws n=my_ws
	$t $(MAKE) -C $(APP) --no-print-directory new t=ranch_protocol n=my_protocol

# Here we disable warnings because templates contain missing behaviors.
	$i "Build the application"
	$t $(MAKE) -C $(APP) ERLC_OPTS=+debug_info $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/my_fsm.beam
	$t test -f $(APP)/ebin/my_server.beam
	$t test -f $(APP)/ebin/my_sup.beam

	$i "Check that all the modules can be loaded"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, Mods = [my_fsm, my_http, my_loop, my_protocol, my_rest, my_server, my_sup, my_ws]} \
			= application:get_key($(APP), modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"

clean-bootstrap: $(BOOTSTRAP_CLEAN_TARGETS)

$(BOOTSTRAP_CLEAN_TARGETS):
	$t rm -rf $(APP_TO_CLEAN)/

########################################
# Legacy tests.

app: app1
	$i "app: Testing the 'app' target."
	$t $(MAKE) -C app1 app $v
	$i "Checking the modules line in the generated .app file."
	$t [ `grep -E "{modules, *\['m'\]}" app1/ebin/app1.app | wc -l` -eq 1 ]
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
	$t $(MAKE) -C app1 distclean $v
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
	$t $(MAKE) -C app1 distclean TEST_DIR=eunit $v
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
	$t $(MAKE) -C app1 distclean TEST_DIR=eunit $v
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
		"dep_edown = git https://github.com/uwiger/edown.git 0.7" \
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
	$t [ "`ls app1/doc/`" = "" ]
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
	$t rm -rf app1 erl_crash.dump

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
	$t \
	if [ "$(1)" = "amqp_client" ]; then \
		$(MAKE) -C app1 RABBITMQ_CLIENT_PATCH=1; \
	elif [ "$(1)" = "rabbit" ]; then \
		$(MAKE) -C app1 RABBITMQ_SERVER_PATCH=1; \
	else \
		$(MAKE) -C app1; \
	fi; \
	if [ $$$$? -ne 0 ]; then \
		echo "$(1): make error" >> pkgs.log; \
	else \
		$(MAKE) -C app1; if [ $$$$? -ne 0 ]; then \
			echo "$(1): re-make error" >> pkgs.log; \
		else \
			find . -type f -name erl_crash.dump; if [ $$$$? -ne 0 ]; then \
				echo "$(1): erl_crash.dump found" >> pkgs.log; \
			else \
				erl +A0 -noinput -boot start_clean -pa app1/deps/*/ebin -eval " \
						Apps = [list_to_atom(App) || \"app1/deps/\" ++ App \
							<- filelib:wildcard(\"app1/deps/*\")], \
						[begin \
							io:format(\"Loading application ~p~n\", [App]), \
							case application:load(App) of \
								{error, _} -> ok; \
								ok -> \
									{ok, Mods} = application:get_key(App, modules), \
									[try io:format(\"  Loading module ~p~n\", [Mod]), \
										{module, Mod} = code:load_file(Mod) \
									catch C:R -> timer:sleep(500), erlang:C(R) \
									end || Mod <- Mods] \
							end \
						end || App <- Apps], \
						halt()."; if [ $$$$? -ne 0 ]; then \
					echo "$(1): load error" >> pkgs.log; \
				fi \
			fi \
		fi \
	fi
endef

PACKAGES = $(foreach pkg,$(sort $(wildcard ../index/*.mk)),$(notdir $(basename $(pkg))))

$(foreach pkg,$(PACKAGES),$(eval $(call pkg_test_target,$(pkg))))

pkgs: $(addprefix pkg-,$(PACKAGES))
	@if [ -f pkgs.log ]; then \
		echo "+-------------------------------+"; \
		echo "| ERRORS WHILE TESTING PACKAGES |"; \
		echo "+-------------------------------+"; \
		cat pkgs.log; \
		exit 33; \
	fi

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
