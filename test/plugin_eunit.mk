# EUnit plugin.

EUNIT_CASES = all apps-only apps-only-error check erl-opts fun mod priv test-dir tests
EUNIT_TARGETS = $(addprefix eunit-,$(EUNIT_CASES))

.PHONY: eunit $(EUNIT_TARGETS)

eunit: $(EUNIT_TARGETS)

eunit-all: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Check that EUnit detects no tests"
	$t $(MAKE) -C $(APP) eunit | grep -q "There were no tests to run."

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Build the project cleanly"
	$t $(MAKE) -C $(APP) clean $v
	$t $(MAKE) -C $(APP) $v

	$i "Check that no EUnit test cases were exported"
	$t $(ERL) -pa $(APP)/ebin -eval 'code:load_file($(APP)), false = erlang:function_exported($(APP), ok_test, 0), halt()'

	$i "Check that EUnit runs tests"
	$t $(MAKE) -C $(APP) eunit | grep -q "Test passed."

	$i "Add a failing test to the module"
	$t printf "%s\n" \
		"-ifdef(TEST)." \
		"bad_test() -> throw(fail)." \
		"-endif." >> $(APP)/src/$(APP).erl

	$i "Check that EUnit errors out"
	$t ! $(MAKE) -C $(APP) eunit $v

eunit-apps-only: build clean

	$i "Create a multi application repository with no root application"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t echo "include erlang.mk" > $(APP)/Makefile

	$i "Create a new application named my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Create a new library named my_lib"
	$t $(MAKE) -C $(APP) new-lib in=my_lib $v

	$i "Check that EUnit detects no tests"
	$t $(MAKE) -C $(APP) eunit | grep -q "There were no tests to run."

	$i "Generate a module containing EUnit tests in my_app"
	$t printf "%s\n" \
		"-module(my_app)." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/apps/my_app/src/my_app.erl

	$i "Generate a module containing EUnit tests in my_lib"
	$t printf "%s\n" \
		"-module(my_lib)." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/apps/my_lib/src/my_lib.erl

	$i "Check that EUnit runs tests"
	$t $(MAKE) -C $(APP) eunit | grep -q "Test passed."

	$i "Check that EUnit runs tests"

eunit-apps-only-error: build clean

	$i "Create a multi application repository with no root application"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t echo "include erlang.mk" > $(APP)/Makefile

	$i "Create a new application named my_app1"
	$t $(MAKE) -C $(APP) new-app in=my_app1 $v

	$i "Create a new application named my_app2"
	$t $(MAKE) -C $(APP) new-app in=my_app2 $v

	$i "Create a new library named my_lib"
	$t $(MAKE) -C $(APP) new-lib in=my_lib $v

	$i "Check that EUnit detects no tests"
	$t $(MAKE) -C $(APP) eunit | grep -q "There were no tests to run."

	$i "Generate a module containing broken EUnit tests in my_app1"
	$t printf "%s\n" \
		"-module(my_app1)." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"bad_test() -> ?assert(0 =:= 1)." \
		"-endif." > $(APP)/apps/my_app1/src/my_app1.erl

	$i "Generate a module containing EUnit good tests in my_app2"
	$t printf "%s\n" \
		"-module(my_app2)." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/apps/my_app2/src/my_app2.erl

	$i "Generate a module containing EUnit tests in my_lib"
	$t printf "%s\n" \
		"-module(my_lib)." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/apps/my_lib/src/my_lib.erl

	$i "Check exit code of EUnit for the module with broken test should be non-zero"
	$t ! $(MAKE) -C $(APP)/apps/my_app1 eunit $v

	$i "Check exit code of EUnit for the whole project with broken test should be non-zero"
	$t ! $(MAKE) -C $(APP) eunit $v

eunit-check: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Check that EUnit runs on 'make check'"
	$t $(MAKE) -C $(APP) check | grep -q "Test passed."

eunit-erl-opts: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Set EUNIT_ERL_OPTS in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "EUNIT_ERL_OPTS = -eval \"erlang:display(hello).\" \n"}' $(APP)/Makefile

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Check that EUnit uses EUNIT_ERL_OPTS"
	$t $(MAKE) -C $(APP) eunit | grep -q "hello"

eunit-fun: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"bad_test() -> throw(fail)." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Check that we can run EUnit on a specific test"
	$t $(MAKE) -C $(APP) eunit t=$(APP):ok_test $v

eunit-mod: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Generate a module containing failing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP)_fail)." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"bad_test() -> throw(fail)." \
		"-endif." > $(APP)/src/$(APP)_fail.erl

	$i "Check that we can run EUnit on a specific module"
	$t $(MAKE) -C $(APP) eunit t=$(APP) $v

eunit-priv: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ?assert(is_list(code:priv_dir($(APP))) =:= true)." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Check that EUnit can resolve the priv_dir"
	$t $(MAKE) -C $(APP) tests | grep -q "Test passed."

eunit-test-dir: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"log_test() -> os:cmd(\"echo $(APP) >> eunit.log\")." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Generate a module containing EUnit tests in TEST_DIR"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_tests)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"log_test() -> os:cmd(\"echo $(APP)_tests >> eunit.log\")." > $(APP)/test/$(APP)_tests.erl

	$i "Check that EUnit runs both tests"
	$t $(MAKE) -C $(APP) eunit | grep -q "2 tests passed."

	$i "Check that tests were both run only once"
	$t printf "%s\n" $(APP) $(APP)_tests | cmp $(APP)/eunit.log -

eunit-tests: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Check that EUnit runs on 'make tests'"
	$t $(MAKE) -C $(APP) tests | grep -q "Test passed."
