# Shell plugin.

SHELL_TARGETS = $(call list_targets,shell)

.PHONY: shell $(C_SRC_TARGETS)

shell: $(SHELL_TARGETS)

shell-default: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Run the shell"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="-eval 'halt()'" $v

shell-kjell: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Kjell to the list of shell dependencies and set as default shell"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "SHELL_DEPS = kjell\nSHELL_ERL = \$$(DEPS_DIR)/kjell/bin/kjell\n"}' $(APP)/Makefile

	$i "Run the shell"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="-eval 'halt()'" $v

shell-test-dir: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate a module in TEST_DIR"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module(foo)." > $(APP)/test/foo.erl

	$i "Check that the module is visible"
	$t $(MAKE) -C $(APP) test-build shell SHELL_OPTS="-eval 'foo:module_info()' -eval 'halt()'" $v
