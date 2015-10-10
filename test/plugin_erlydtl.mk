# ErlyDTL plugin

ERLYDTL_CASES = test
ERLYDTL_TARGETS = $(addprefix erlydtl-,$(ERLYDTL_CASES))
ERLYDTL_CLEAN_TARGETS = $(addprefix clean-,$(ERLYDTL_TARGETS))

.PHONY: erlydtl clean-erlydtl

clean-erlydtl: $(ERLYDTL_CLEAN_TARGETS)

$(ERLYDTL_CLEAN_TARGETS):
	$t rm -rf $(APP_TO_CLEAN)/

erlydtl: $(ERLYDTL_TARGETS)

erlydtl-test: build clean-erlydtl-test
	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v
	$t mkdir $(APP)/templates
	$t touch $(APP)/templates/$(APP)_template.dtl
	$t touch $(APP)/templates/$(APP)_template2.dtl
	$t echo "PROJECT = $(APP)" > $(APP)/Makefile
	$t echo "DEPS = erlydtl" >> $(APP)/Makefile
	$t echo "include erlang.mk" >> $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that ErlyDTL templates are compiled"
	$t test -f $(APP)/ebin/$(APP)_template_dtl.beam
	$t test -f $(APP)/ebin/$(APP)_template2_dtl.beam

	$i "Check that ErlyDTL generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, Modules} = application:get_key($(APP), modules), \
		true = lists:member($(APP)_template_dtl, Modules), \
		true = lists:member($(APP)_template2_dtl, Modules), \
		halt()"
