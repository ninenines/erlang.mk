# Triq plugin

TRIQ_CASES = test-dir
TRIQ_TARGETS = $(addprefix triq-,$(TRIQ_CASES))

.PHONY: triq $(TRIQ_TARGETS)

triq: $(TRIQ_TARGETS)

triq-test-dir: build clean
	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add Triq to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = triq\n"}' $(APP)/Makefile

	$i "Generate a module containing Triq properties"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"triq/include/triq.hrl\")." \
		"prop_foo() -> ?FORALL(_, any(), true)." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Generate a module containing Triq properies in TEST_DIR"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_tests)." \
		"-include_lib(\"triq/include/triq.hrl\")." \
		"prop_bar() -> ?FORALL(_, any(), true)." > $(APP)/test/$(APP)_tests.erl

	$i "Run the Triq plugin"
	$t $(MAKE) -C $(APP) triq > $(APP)/triq.log

	$i "Check that both properties were checked"
	$t grep -q prop_foo $(APP)/triq.log
	$t grep -q prop_bar $(APP)/triq.log
