# Core: COMPILE_FIRST dependencies generation.

core_makedep_TARGETS = $(call list_targets,core-makedep)

.PHONY: core-makedep $(core_makedep_TARGETS)

core-makedep: $(core_makedep_TARGETS)

core-makedep-behavior: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v
	$t mkdir $(APP)/src/core

	$i "Generate related .erl files"
	$t printf "%s\n" "-module(human)." "-callback live() -> 'ok'." > $(APP)/src/core/human.erl
	$t printf "%s\n" "-module(boy)." "-behavior(human)." "-export([live/0])." "live() -> ok." > $(APP)/src/boy.erl
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/boy.beam
	$t test -f $(APP)/ebin/human.beam

core-makedep-ignore-filenames-with-spaces: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v
	$t mkdir $(APP)/src/core

	$i "Generate related .erl files"
	$t printf "%s\n" "-module(human)." "-export([live/0])." "live() -> ok." > $(APP)/src/core/human.erl
	$t printf "%s\n" "-module(boy)." "-import(human,[live/0])." > $(APP)/src/boy.erl
	$t printf "%s\n" "-module(boy)." "-import(human,[live/0])." > $(APP)/src/my\ boy.erl
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/boy.beam
	$t test -f $(APP)/ebin/human.beam

core-makedep-ignore-special-files: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v
	$t mkdir $(APP)/src/core

	$i "Generate related .erl files"
	$t printf "%s\n" "-module(human)." "-export([live/0])." "live() -> ok." > $(APP)/src/core/human.erl
	$t printf "%s\n" "-module(boy)." "-import(human,[live/0])." > $(APP)/src/boy.erl
	$t printf "%s\n" "-module(boy)." "-import(human,[live/0])." > $(APP)/src/.#boy.erl
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/boy.beam
	$t test -f $(APP)/ebin/human.beam

core-makedep-import: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v
	$t mkdir $(APP)/src/core

	$i "Generate related .erl files"
	$t printf "%s\n" "-module(human)." "-export([live/0])." "live() -> ok." > $(APP)/src/core/human.erl
	$t printf "%s\n" "-module(boy)." "-import(human,[live/0])." > $(APP)/src/boy.erl
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/boy.beam
	$t test -f $(APP)/ebin/human.beam

	$i "Confirm the file was added by makedep"
	$t grep COMPILE_FIRST $(APP)/$(APP).d | grep -q core/human

core-makedep-non-usascii-paths: NON_USASCII_DIR = $(APP)/héhé
core-makedep-non-usascii-paths: init

	$i "Create working directory with non-US-ASCII characters"
	$t mkdir -p $(NON_USASCII_DIR)

	$i "Bootstrap a new OTP library named my_dep"
	$t mkdir $(NON_USASCII_DIR)/my_dep/
	$t cp ../erlang.mk $(NON_USASCII_DIR)/my_dep/
	$t $(MAKE) -C $(NON_USASCII_DIR)/my_dep -f erlang.mk bootstrap-lib $v
	$t mkdir $(NON_USASCII_DIR)/my_dep/include

	$i "Bootstrap a new OTP application named my_app"
	$t mkdir $(NON_USASCII_DIR)/my_app/
	$t cp ../erlang.mk $(NON_USASCII_DIR)/my_app/
	$t $(MAKE) -C $(NON_USASCII_DIR)/my_app -f erlang.mk bootstrap $v

	$i "Add my_dep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = cp $(CURDIR)/$(NON_USASCII_DIR)/my_dep/\n"}' $(NON_USASCII_DIR)/my_app/Makefile

	$i "Generate related .hrl/.erl files"
	$t printf "%s\n" "-define(HELLO, hello)." > $(NON_USASCII_DIR)/my_dep/include/hello.hrl
	$t printf "%s\n" "-module(hello)." "-include_lib(\"my_dep/include/hello.hrl\")." "-export([hello/0])." "hello() -> ?HELLO." > $(NON_USASCII_DIR)/my_app/src/hello.erl
	$t $(MAKE) -C $(NON_USASCII_DIR)/my_app $v

	$i "Check that all compiled files exist"
	$t test -f $(NON_USASCII_DIR)/my_app/my_app.d
	$t grep -qw $(NON_USASCII_DIR) $(NON_USASCII_DIR)/my_app/my_app.d
	$t test -f $(NON_USASCII_DIR)/my_app/ebin/my_app.app
	$t test -f $(NON_USASCII_DIR)/my_app/ebin/hello.beam
