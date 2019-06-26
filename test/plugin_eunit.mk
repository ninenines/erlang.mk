# EUnit plugin.

EUNIT_TARGETS = $(call list_targets,eunit)

.PHONY: eunit $(EUNIT_TARGETS)

eunit: $(EUNIT_TARGETS)

eunit-all: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Check that EUnit detects no tests"
	$t $(MAKE) -C $(APP) eunit | grep -c "There were no tests to run." | grep -q 1

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
	$t $(MAKE) -C $(APP) eunit | grep -c "Test passed." | grep -q 1

	$i "Add a failing test to the module"
	$t printf "%s\n" \
		"-ifdef(TEST)." \
		"bad_test() -> throw(fail)." \
		"-endif." >> $(APP)/src/$(APP).erl

	$i "Check that EUnit errors out"
	$t ! $(MAKE) -C $(APP) eunit $v

eunit-apps-include-lib: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create new libraries the_app and test_helpers"
	$t $(MAKE) -C $(APP) new-lib in=the_app $v
	$t $(MAKE) -C $(APP) new-lib in=test_helpers $v

	$i "Generate .erl file"
	$t echo '-module(the).  -export([thing/0]).  thing() -> true.' > $(APP)/apps/the_app/src/the.erl

	$t mkdir -p $(APP)/apps/the_app/test
	$i "Generate test .erl file with helper include_lib()"
	$t echo '-module(the_test).' > $(APP)/apps/the_app/test/the_test.erl
	$t echo '-include_lib("eunit/include/eunit.hrl").' >> $(APP)/apps/the_app/test/the_test.erl
	$t echo '-include_lib("test_helpers/include/test_helpers.hrl").' >> $(APP)/apps/the_app/test/the_test.erl
	$t echo 'thing_test() -> ?assertEqual(true, the:thing()).' >> $(APP)/apps/the_app/test/the_test.erl

	$t mkdir -p $(APP)/apps/test_helpers/include
	$t echo '%% test_helpers'  > $(APP)/apps/test_helpers/include/test_helpers.hrl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Run eunit"
	$t $(MAKE) -C $(APP) eunit $v

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Run eunit on a subdirectory"
	$t $(MAKE) -C $(APP)/apps/the_app eunit $v

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

eunit-apps-include-lib-deps: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create new library the_app"
	$t $(MAKE) -C $(APP) new-lib in=the_app $v

	$i "Add Cowlib to the list of dependencies of the_app"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib_commit = master\n"}' $(APP)/apps/the_app/Makefile

	$i "Generate .erl file that uses include_lib()"
	$t echo '-module(the).  -include_lib("cowlib/include/cow_parse.hrl").  -export([thing/0]).  thing() -> true.' > $(APP)/apps/the_app/src/the.erl

	$i "Run eunit"
	$t $(MAKE) -C $(APP) eunit $v

eunit-apps-one-app-tested: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a new application named my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Create a new library named my_lib"
	$t $(MAKE) -C $(APP) new-lib in=my_lib $v

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

	$i "Run EUnit on my_app only"
	$t $(MAKE) -C $(APP)/apps/my_app eunit | grep -c "Test passed." | grep -q 1

eunit-apps-only: init

	$i "Create a multi application repository with no root application"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t echo "include erlang.mk" > $(APP)/Makefile

	$i "Create a new application named my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Create a new library named my_lib"
	$t $(MAKE) -C $(APP) new-lib in=my_lib $v

	$i "Check that EUnit detects no tests"
	$t $(MAKE) -C $(APP) eunit | grep -c "There were no tests to run." | grep -q 2

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
	$t $(MAKE) -C $(APP) eunit | grep -c "Test passed." | grep -q 2

eunit-apps-only-error: init

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
	$t $(MAKE) -C $(APP) eunit | grep -c "There were no tests to run." | grep -q 3

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

eunit-check: init

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
	$t $(MAKE) -C $(APP) check | grep -c "Test passed." | grep -q 1

eunit-erl-opts: init

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
	$t $(MAKE) -C $(APP) eunit | grep -c "hello" | grep -q 1

eunit-fun: init

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

eunit-mod: init

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

eunit-priv: init

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
	$t $(MAKE) -C $(APP) tests | grep -c "Test passed." | grep -q 1

eunit-test-dir: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"log_test() -> file:write_file(\"eunit.log\", \"$(APP)\n\", [append])." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Generate a module containing EUnit tests in TEST_DIR"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_tests)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"log_test() -> file:write_file(\"eunit.log\", \"$(APP)_tests\n\", [append])." > $(APP)/test/$(APP)_tests.erl

	$i "Check that EUnit runs both tests"
	$t $(MAKE) -C $(APP) eunit | grep -c "2 tests passed." | grep -q 1

	$i "Check that tests were both run only once"
	$t printf "%s\n" $(APP) $(APP)_tests | cmp $(APP)/eunit.log -

eunit-tests: init

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
	$t $(MAKE) -C $(APP) tests | grep -c "Test passed." | grep -q 1
