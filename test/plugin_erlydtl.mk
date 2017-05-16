# ErlyDTL plugin.

ERLYDTL_CASES = compile custom-tag full-path include-template opts path-full-path-suffix suffix
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

erlydtl-custom-tag: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add ErlyDTL to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = erlydtl\n"}' $(APP)/Makefile

	$i "Generate an ErlyDTL library module containing a custom tag"
	$t printf "%s\n" \
		"-module($(APP)_lib)." \
		"-behavior(erlydtl_library)." \
		"-export([version/0, inventory/1, mytag/2])." \
		"version() -> 1." \
		"inventory(filters) -> [];" \
		"inventory(tags) -> [mytag]." \
		"mytag(_,_) -> <<\"hello\">>." > $(APP)/src/$(APP)_lib.erl

	$i "Add our library to the ErlyDTL options"
	$t echo "DTL_OPTS = {libraries, [{erlang_mk, $(APP)_lib}]}" >> $(APP)/Makefile

	$i "Generate an ErlyDTL template that uses a custom tag"
	$t mkdir $(APP)/templates/
	$t printf "%s\n" \
		"{% load erlang_mk %}" \
		"{% mytag as value %}" \
		"{{ value }}" > $(APP)/templates/$(APP).dtl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the ErlyDTL template is compiled"
	$t test -f $(APP)/ebin/$(APP)_dtl.beam

	$i "Check that ErlyDTL generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [$(APP_)_dtl, $(APP)_lib]} = application:get_key($(APP), modules), \
		halt()"

	$i "Confirm the custom tag is used"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval \
		'file:write_file("$(APP)/OUT", element(2, $(APP)_dtl:render())), halt()'
	$t cat $(APP)/OUT | grep -q hello

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

erlydtl-include-template: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate ErlyDTL templates"
	$t mkdir -p $(APP)/dtl/foo/
	$t echo '{% include "foo/bar.dtl" %}' > $(APP)/dtl/foo.dtl
	$t echo '{% var %}' > $(APP)/dtl/foo/bar.dtl
	$t echo '{% extends "bar.dtl" %}' > $(APP)/dtl/foo/baz.dtl

	$i "Build the application"
	$t $(MAKE) -C $(APP) DEPS=erlydtl DTL_PATH=dtl DTL_FULL_PATH=1 DTL_SUFFIX= $v

	$i "Check that ErlyDTL templates are compiled"
	$t test -f $(APP)/ebin/foo.beam
	$t test -f $(APP)/ebin/foo_bar.beam
	$t test -f $(APP)/ebin/foo_baz.beam

	$i "Check that ErlyDTL generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [foo, foo_bar, foo_baz]} = application:get_key($(APP), modules), \
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

erlydtl-path-full-path-suffix: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate ErlyDTL templates"
	$t mkdir -p $(APP)/dtl/two/
	$t touch $(APP)/dtl/one.dtl
	$t touch $(APP)/dtl/two/three.dtl

	$i "Build the application"
	$t $(MAKE) -C $(APP) DEPS=erlydtl DTL_PATH=dtl DTL_FULL_PATH=1 DTL_SUFFIX=_suffix $v

	$i "Check that ErlyDTL templates are compiled"
	$t test -f $(APP)/ebin/one_suffix.beam
	$t test -f $(APP)/ebin/two_three_suffix.beam

	$i "Check that ErlyDTL generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [one_suffix, two_three_suffix]} = application:get_key($(APP), modules), \
		halt()"

erlydtl-suffix: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate ErlyDTL templates"
	$t mkdir $(APP)/templates/
	$t touch $(APP)/templates/one.dtl
	$t touch $(APP)/templates/two.dtl

	$i "Build the application"
	$t $(MAKE) -C $(APP) DEPS=erlydtl DTL_SUFFIX= $v

	$i "Check that ErlyDTL templates are compiled"
	$t test -f $(APP)/ebin/one.beam
	$t test -f $(APP)/ebin/two.beam

	$i "Check that ErlyDTL generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [one, two]} = application:get_key($(APP), modules), \
		halt()"
