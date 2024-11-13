# Common Test plugin.

ct_TARGETS = $(call list_targets,ct)

.PHONY: ct $(ct_TARGETS)

ct: $(ct_TARGETS)

ct-all: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Check that Common Test detects no tests"
	$t $(MAKE) -C $(APP) ct | grep -c "Nothing to be done for" | grep -q 1

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> ok." > $(APP)/test/$(APP)_SUITE.erl

	$i "Check that Common Test runs tests"
# We can't pipe CT's output without it crashing, so let's check that
# the command succeeds and log files are created instead.
	$t test ! -e $(APP)/logs/index.html
	$t $(MAKE) -C $(APP) ct $v
	$t test -f $(APP)/logs/index.html

	$i "Generate a Common Test suite with a failing test case"
	$t printf "%s\n" \
		"-module($(APP)_fail_SUITE)." \
		"-export([all/0, fail/1])." \
		"all() -> [fail]." \
		"fail(_) -> throw(fail)." > $(APP)/test/$(APP)_fail_SUITE.erl

	$i "Check that Common Test errors out"
	$t ! $(MAKE) -C $(APP) ct $v

	$i "Check that logs are kept on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test -f $(APP)/logs/index.html

	$i "Check that logs are deleted on distclean"
	$t $(MAKE) -C $(APP) distclean $v
	$t test ! -e $(APP)/logs/index.html

ct-apps: init

	$i "Create a multi application repository with root application"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/.
	$t echo "include erlang.mk" > $(APP)/Makefile

	$i "Create a new library named my_lib"
	$t $(MAKE) -C $(APP) new-lib in=my_lib $v

	$i "Populate my_lib"
	$t printf "%s\n" \
		"-module(my_lib)." \
		"-export([random_int/0])." \
		"random_int() -> 4." > $(APP)/apps/my_lib/src/my_lib.erl

	$i "Generate a Common Test suite in root application"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module(my_root_SUITE)." \
		"-export([all/0, ok/1, call_my_lib/1])." \
		"all() -> [ok, call_my_lib]." \
		"ok(_) -> ok." \
		"call_my_lib(_) -> 4 = my_lib:random_int()." > $(APP)/test/my_root_SUITE.erl

	$i "Check that Common Test runs tests"
	$t $(MAKE) -C $(APP) ct $v

	$i "Check that Common Test runs tests from a specific test suite"
	$t $(MAKE) -C $(APP) ct-my_root $v

	$i "Check that Common Test runs tests from a specific test suite using CT_SUITES"
	$t $(MAKE) -C $(APP) ct CT_SUITES=my_root $v

ct-apps-only: init

	$i "Create a multi application repository with no root application"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t echo "include erlang.mk" > $(APP)/Makefile

	$i "Create a new application named my_app_only"
	$t $(MAKE) -C $(APP) new-app in=my_app_only $v

	$i "Create a new library named my_lib_only"
	$t $(MAKE) -C $(APP) new-lib in=my_lib_only $v

	$i "Populate my_lib_only"
	$t printf "%s\n" \
		"-module(my_lib_only)." \
		"-export([random_int/0])." \
		"random_int() -> 4." > $(APP)/apps/my_lib_only/src/my_lib_only.erl

	$i "Check that Common Test detects no tests"
	$t $(MAKE) -C $(APP) ct | grep -c "Nothing to be done for" | grep -q 2

	$i "Generate a Common Test suite in my_app_only"
	$t mkdir $(APP)/apps/my_app_only/test
	$t printf "%s\n" \
		"-module(my_app_only_SUITE)." \
		"-export([all/0, ok/1, call_my_lib_only/1])." \
		"all() -> [ok, call_my_lib_only]." \
		"ok(_) -> ok." \
		"call_my_lib_only(_) -> 4 = my_lib_only:random_int()." > $(APP)/apps/my_app_only/test/my_app_only_SUITE.erl

	$i "Generate a Common Test suite in my_lib_only"
	$t mkdir $(APP)/apps/my_lib_only/test
	$t printf "%s\n" \
		"-module(my_lib_only_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> ok." > $(APP)/apps/my_lib_only/test/my_lib_only_SUITE.erl

	$i "Check that Common Test runs tests"
# We can't pipe CT's output without it crashing, so let's check that
# the command succeeds and log files are created instead.
	$t test ! -e $(APP)/apps/my_app_only/logs/index.html
	$t test ! -e $(APP)/apps/my_lib_only/logs/index.html
	$t $(MAKE) -C $(APP) ct $v
	$t test -f $(APP)/apps/my_app_only/logs/index.html
	$t test -f $(APP)/apps/my_lib_only/logs/index.html

ct-case: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a Common Test suite with two cases"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, groups/0, ok/1, bad/1])." \
		"all() -> [{group, mygroup}]." \
		"groups() -> [{mygroup, [ok, bad]}]." \
		"ok(_) -> ok." \
		"bad(_) -> throw(fail)." > $(APP)/test/$(APP)_SUITE.erl

	$i "Check that we can run Common Test on a specific test case"
	$t $(MAKE) -C $(APP) ct-$(APP) t=mygroup:ok $v

ct-case-without-group: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a Common Test suite with two cases that are not part of any group"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1, bad/1])." \
		"all() -> [ok, bad]." \
		"ok(_) -> ok." \
		"bad(_) -> throw(fail)." > $(APP)/test/$(APP)_SUITE.erl

	$i "Check that we can run Common Test on a specific test case"
	$t $(MAKE) -C $(APP) ct-$(APP) c=ok $v

ct-check: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> ok." > $(APP)/test/$(APP)_SUITE.erl

	$i "Check that Common Test runs on 'make check'"
	$t test ! -e $(APP)/logs/index.html
	$t $(MAKE) -C $(APP) check $v
	$t test -f $(APP)/logs/index.html

ct-group: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a Common Test suite with two groups"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, groups/0, ok/1, bad/1])." \
		"all() -> [{group, okgroup}, {group, badgroup}]." \
		"groups() -> [{okgroup, [ok]}, {badgroup, [bad]}]." \
		"ok(_) -> ok." \
		"bad(_) -> throw(fail)." > $(APP)/test/$(APP)_SUITE.erl

	$i "Check that we can run Common Test on a specific group"
	$t $(MAKE) -C $(APP) ct-$(APP) t=okgroup $v

ct-opts: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Set CT_OPTS in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "CT_OPTS = -label hello_ct_opts\n"}' $(APP)/Makefile

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> ok." > $(APP)/test/$(APP)_SUITE.erl

	$i "Run Common Test"
	$t $(MAKE) -C $(APP) ct $v

	$i "Check that Common Test uses options from CT_OPTS"
	$t grep -q hello_ct_opts $(APP)/logs/index.html

ct-logs-dir: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Set CT_OPTS in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "CT_LOGS_DIR = custom_dir\n"}' $(APP)/Makefile

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> ok." > $(APP)/test/$(APP)_SUITE.erl

	$i "Check that Common Test log in right place"
	$t test ! -e $(APP)/custom_dir/index.html
	$t $(MAKE) -C $(APP) ct $v
	$t test -f $(APP)/custom_dir/index.html

ct-suite: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate two Common Test suites"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_ok_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> ok." > $(APP)/test/$(APP)_ok_SUITE.erl
	$t printf "%s\n" \
		"-module($(APP)_fail_SUITE)." \
		"-export([all/0, bad/1])." \
		"all() -> [bad]." \
		"bad(_) -> throw(fail)." > $(APP)/test/$(APP)_fail_SUITE.erl

	$i "Check that we can run Common Test on a specific test suite"
	$t $(MAKE) -C $(APP) ct-$(APP)_ok $v

ct-tests: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> ok." > $(APP)/test/$(APP)_SUITE.erl

	$i "Check that Common Test runs on 'make tests'"
	$t test ! -e $(APP)/logs/index.html
	$t $(MAKE) -C $(APP) tests $v
	$t test -f $(APP)/logs/index.html
