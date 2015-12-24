# C source plugin.

C_SRC_CASES = cpp custom dir env nif port
C_SRC_TARGETS = $(addprefix c-src-,$(C_SRC_CASES))

.PHONY: c-src $(C_SRC_TARGETS)

c-src: $(C_SRC_TARGETS)
c_src: c-src

c-src-nif: build clean

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
ifeq ($(PLATFORM),msys2)
	$t test -f $(APP)/priv/$(APP).dll
else
	$t test -f $(APP)/priv/$(APP).so
endif

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
ifeq ($(PLATFORM),msys2)
	$t test -f $(APP)/priv/$(APP).dll
else
	$t test -f $(APP)/priv/$(APP).so
endif

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
ifeq ($(PLATFORM),msys2)
	$t test ! -e $(APP)/priv/$(APP).dll
else
	$t test ! -e $(APP)/priv/$(APP).so
endif

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that all files were removed"
	$t test ! -e $(APP)/c_src/env.mk
