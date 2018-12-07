# PropEr plugin.

PROPER_TARGETS = $(call list_targets,proper)

.PHONY: proper $(PROPER_TARGETS)

proper: $(PROPER_TARGETS)

proper-test-dir: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add PropEr to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = proper\n"}' $(APP)/Makefile

	$i "Generate a module containing PropEr properties"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"proper/include/proper.hrl\")." \
		"prop_foo() -> ?FORALL(_, any(), true)." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Generate a module containing PropEr properties in TEST_DIR"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_tests)." \
		"-include_lib(\"proper/include/proper.hrl\")." \
		"prop_bar() -> ?FORALL(_, any(), true)." > $(APP)/test/$(APP)_tests.erl

	$i "Run the PropEr plugin"
	$t $(MAKE) -C $(APP) proper $v > $(APP)/proper.log

	$i "Check that both properties were checked"
	$t grep -q prop_foo $(APP)/proper.log
	$t grep -q prop_bar $(APP)/proper.log
