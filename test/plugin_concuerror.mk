# Concuerror plugin.

concuerror_TARGETS = $(call list_targets,concuerror)

.PHONY: concuerror $(concuerror_TARGETS)

concuerror: $(concuerror_TARGETS)

concuerror-app: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a test module with a function that returns immediately"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module(concuerror_success)." \
		"-export([test/0])." \
		"test() -> ok." > $(APP)/test/concuerror_success.erl

	$i "Add the test case to CONCUERROR_TESTS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "CONCUERROR_TESTS += concuerror_success:test\n"}' $(APP)/Makefile

	$i "Confirm that Concuerror completes successfully"
	$t $(MAKE) -C $(APP) concuerror $v

	$i "Create a test module with a function that has no local return"
	$t printf "%s\n" \
		"-module(concuerror_error)." \
		"-export([test/0])." \
		"test() -> 1 = 2, ok." > $(APP)/test/concuerror_error.erl

	$i "Add the test case to CONCUERROR_TESTS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "CONCUERROR_TESTS += concuerror_error:test\n"}' $(APP)/Makefile

	$i "Confirm that Concuerror errors out"
	$t ! $(MAKE) -C $(APP) concuerror $v
