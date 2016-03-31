# ErlyDTL plugin.

ERLYDTL_CASES = compile full-path opts
ERLYDTL_TARGETS = $(addprefix erlydtl-,$(ERLYDTL_CASES))

.PHONY: erlydtl $(ERLYDTL_TARGETS)

erlydtl: $(ERLYDTL_TARGETS)

erlydtl-compile: build clean

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

erlydtl-full-path: build clean

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

erlydtl-opts: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add ErlyDTL to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = erlydtl\n"}' $(APP)/Makefile

	$i "Generate ErlyDTL template"
	$t mkdir $(APP)/templates/
	$t echo "{{ foo }}" > $(APP)/templates/$(APP)_foo.dtl

	$i "Build the application with auto escape turned on"
	$t $(MAKE) -C $(APP) DTL_OPTS=auto_escape $v

	$i "Check that HTML is escaped"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/erlydtl/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, Result} = $(APP)_foo_dtl:render([{foo, <<\"<&>\">>}]), \
		<<\"&lt;&amp;&gt;\", _/binary>> = iolist_to_binary(Result), \
		halt()"

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Build the application with auto escape turned off"
	$t $(MAKE) -C $(APP) "DTL_OPTS={auto_escape, false}" $v

	$i "Check that HTML is not escaped"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/erlydtl/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, Result} = $(APP)_foo_dtl:render([{foo, <<\"<&>\">>}]), \
		<<\"<&>\", _/binary>> = iolist_to_binary(Result), \
		halt()"
