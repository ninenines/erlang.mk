# C source plugin.

c_src_TARGETS = $(call list_targets,c-src)

.PHONY: c-src $(c_src_TARGETS)

c-src: $(c_src_TARGETS)
c_src: c-src

$(info PLATFORM=$(PLATFORM))
ifeq ($(PLATFORM),msys2)
C_SRC_OUTPUT_SHARED_EXTENSION = .dll
C_SRC_OUTPUT_STATIC_EXTENSION = .lib
else
C_SRC_OUTPUT_SHARED_EXTENSION = .so
C_SRC_OUTPUT_STATIC_EXTENSION = .a
endif

c-src-makefile-change: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate a NIF from templates"
	$t $(MAKE) -C $(APP) new-nif n=$(APP) $v

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Touch the Makefile; check that all files get rebuilt"
	$t printf "%s\n" \
		$(APP)/$(APP).d \
		$(APP)/c_src/$(APP).o \
		$(APP)/ebin/$(APP).app \
		$(APP)/ebin/$(APP).beam \
		$(APP)/priv/$(APP)$(C_SRC_OUTPUT_SHARED_EXTENSION) \
		$(APP)/src/$(APP).erl | sort > $(APP)/EXPECT
	$t $(SLEEP)
	$t touch $(APP)/Makefile
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) $v
	$t find $(APP) -type f -newer $(APP)/Makefile -not -path "$(APP)/.erlang.mk/*" | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT

c-src-nif: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate a NIF from templates"
	$t $(MAKE) -C $(APP) new-nif n=$(APP) $v

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/c_src/$(APP).o
	$t test -f $(APP)/c_src/env.mk
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP).beam
	$t test -f $(APP)/priv/$(APP)$(C_SRC_OUTPUT_SHARED_EXTENSION)

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, [$(APP)]} = application:get_key($(APP), modules), \
		{module, $(APP)} = code:load_file($(APP)), \
		{hello, joe} = $(APP):hello(joe), \
		{hello, mike} = $(APP):hello(mike), \
		{hello, robert} = $(APP):hello(robert), \
		halt()"

	$i "Re-build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/c_src/$(APP).o
	$t test -f $(APP)/c_src/env.mk
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP).beam
	$t test -f $(APP)/priv/$(APP)$(C_SRC_OUTPUT_SHARED_EXTENSION)

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, [$(APP)]} = application:get_key($(APP), modules), \
		{module, $(APP)} = code:load_file($(APP)), \
		{hello, joe} = $(APP):hello(joe), \
		{hello, mike} = $(APP):hello(mike), \
		{hello, robert} = $(APP):hello(robert), \
		halt()"

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that all intermediate files were removed"
	$t test ! -e $(APP)/$(APP).d
	$t test ! -e $(APP)/c_src/$(APP).o
	$t test ! -e $(APP)/ebin/$(APP).app
	$t test ! -e $(APP)/ebin/$(APP).beam
	$t test ! -e $(APP)/priv/$(APP)$(C_SRC_OUTPUT_SHARED_EXTENSION)

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that all files were removed"
	$t test ! -e $(APP)/c_src/env.mk

c-src-nif-missing-name: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Try to generate a NIF without giving it a name"
	$t ! $(MAKE) -C $(APP) new-nif $v

c-src-static-nif: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate a NIF from templates"
	$t $(MAKE) -C $(APP) new-nif n=$(APP) $v

	$i "Set C_SRC_TYPE = static in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "C_SRC_TYPE = static\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/c_src/$(APP).o
	$t test -f $(APP)/c_src/env.mk
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP).beam
	$t test -f $(APP)/priv/$(APP)$(C_SRC_OUTPUT_STATIC_EXTENSION)

	$i "Check that the library file contains the expected functions"
	$t objdump -t $(APP)/priv/test_c_src_static_nif.a | grep -c hello | grep -q 1

	$i "Re-build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/c_src/$(APP).o
	$t test -f $(APP)/c_src/env.mk
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP).beam
	$t test -f $(APP)/priv/$(APP)$(C_SRC_OUTPUT_STATIC_EXTENSION)

	$i "Check that the library file contains the expected functions"
	$t objdump -t $(APP)/priv/test_c_src_static_nif.a | grep -c hello | grep -q 1

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that all intermediate files were removed"
	$t test ! -e $(APP)/$(APP).d
	$t test ! -e $(APP)/c_src/$(APP).o
	$t test ! -e $(APP)/ebin/$(APP).app
	$t test ! -e $(APP)/ebin/$(APP).beam
	$t test ! -e $(APP)/priv/$(APP)$(C_SRC_OUTPUT_STATIC_EXTENSION)

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that all files were removed"
	$t test ! -e $(APP)/c_src/env.mk
