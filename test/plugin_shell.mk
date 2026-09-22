# Shell plugin.

shell_TARGETS = $(call list_targets,shell)

.PHONY: shell $(shell_TARGETS)

shell: $(shell_TARGETS)

shell-compile: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Ensure our application is recompiled before the shell runs"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "shell:: app\n"}' $(APP)/Makefile

	$i "Run the shell"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="-eval 'halt()'" $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP)_app.beam
	$t test -f $(APP)/ebin/$(APP)_sup.beam

shell-default: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Run the shell"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="-eval 'halt()'" $v

shell-observer-cli: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add observer_cli to the list of shell dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "SHELL_DEPS = observer_cli\ndep_observer_cli = hex 2.0.0\n"}' $(APP)/Makefile

	$i "Run the shell"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="-eval 'ok = application:load(observer_cli), halt()'" $v

shell-test-dir: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate a module in TEST_DIR"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module(foo)." > $(APP)/test/foo.erl

	$i "Build the test files"
	$t $(MAKE) -C $(APP) test-build $v

	$i "Check that the module is visible"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="-eval 'foo:module_info()' -eval 'halt()'" $v

comma := ,

shell-reload: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate a module and compile it"
	$t printf "%s\n" "-module(foo)." "-export([v/0])." "v() -> 1." > $(APP)/src/foo.erl
	$t $(MAKE) -C $(APP) app $v

	$i "Start a shell that reloads changed modules and reports the new value"
	$t $(MAKE) -C $(APP) shell RELOAD=1 SHELL_OPTS="-noinput -eval 'code:load_file(foo), io:format(\"shell ready~n\"), spawn(fun F() -> case foo:v() of 2 -> io:format(\"reloaded 2~n\"), halt(); _ -> receive after 1000 -> F() end end end).'" > $(APP)/shell.log 2>&1 &
	$t $(call wait_for_success,grep -q 'shell ready' $(APP)/shell.log)

	$i "Change the module and rebuild from another make"
	$t $(SLEEP)
	$t printf "%s\n" "-module(foo)." "-export([v/0])." "v() -> 2." > $(APP)/src/foo.erl
	$t $(MAKE) -C $(APP) app $v

	$i "Check that the running shell reloaded it and printed the list"
	$t $(call wait_for_success,tr -d '\r' < $(APP)/shell.log | grep -q '\[{module$(comma)foo}\]')
	$t $(call wait_for_success,tr -d '\r' < $(APP)/shell.log | grep -q 'reloaded 2')
