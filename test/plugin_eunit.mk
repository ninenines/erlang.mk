# EUnit plugin.

EUNIT_CASES = all apps-only check fun mod test-dir tests
EUNIT_TARGETS = $(addprefix eunit-,$(EUNIT_CASES))
EUNIT_CLEAN_TARGETS = $(addprefix clean-,$(EUNIT_TARGETS))

.PHONY: eunit $(EUNIT_TARGETS) clean-eunit $(EUNIT_CLEAN_TARGETS)

clean-eunit: $(EUNIT_CLEAN_TARGETS)

$(EUNIT_CLEAN_TARGETS):
	$t rm -rf $(APP_TO_CLEAN)

eunit: $(EUNIT_TARGETS)

eunit-all: build clean-eunit-all

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

eunit-apps-only: build clean-eunit-apps-only

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

eunit-check: build clean-eunit-check

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

eunit-fun: build clean-eunit-fun

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

eunit-mod: build clean-eunit-mod

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

eunit-test-dir: build clean-eunit-test-dir

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

eunit-tests: build clean-eunit-tests

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
