# ErlyDTL plugin.

ERLYDTL_CASES = compile full-path
ERLYDTL_TARGETS = $(addprefix erlydtl-,$(ERLYDTL_CASES))
ERLYDTL_CLEAN_TARGETS = $(addprefix clean-,$(ERLYDTL_TARGETS))

.PHONY: erlydtl $(ERLYDTL_TARGETS) clean-erlydtl $(ERLYDTL_CLEAN_TARGETS)

clean-erlydtl: $(ERLYDTL_CLEAN_TARGETS)

$(ERLYDTL_CLEAN_TARGETS):
	$t rm -fr $(APP_TO_CLEAN)/

erlydtl: $(ERLYDTL_TARGETS)

erlydtl-compile: build clean-erlydtl-compile

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add ErlyDTL to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = erlydtl\n"}' $(APP)/Makefile

	$i "Generate ErlyDTL templates"
	$t mkdir $(APP)/templates/
	$t touch $(APP)/templates/$(APP)_one.dtl
	$t touch $(APP)/templates/$(APP)_two.dtl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that ErlyDTL templates are compiled"
	$t test -f $(APP)/ebin/$(APP)_one_dtl.beam
	$t test -f $(APP)/ebin/$(APP)_two_dtl.beam

	$i "Check that ErlyDTL generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [$(APP_)_one_dtl, $(APP)_two_dtl]} = application:get_key($(APP), modules), \
		halt()"

erlydtl-full-path: build clean-erlydtl-full-path

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add ErlyDTL to the list of dependencies; set DTL_FULL_PATH"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = erlydtl\nDTL_FULL_PATH = 1\n"}' $(APP)/Makefile

	$i "Generate ErlyDTL templates"
	$t mkdir -p $(APP)/templates/deep/
	$t touch $(APP)/templates/$(APP)_one.dtl
	$t touch $(APP)/templates/deep/$(APP)_two.dtl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that ErlyDTL templates are compiled"
	$t test -f $(APP)/ebin/$(APP)_one_dtl.beam
	$t test -f $(APP)/ebin/deep_$(APP)_two_dtl.beam

	$i "Check that ErlyDTL generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [deep_$(APP)_two_dtl, $(APP_)_one_dtl]} = application:get_key($(APP), modules), \
		halt()"
