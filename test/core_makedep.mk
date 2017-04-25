# Core: COMPILE_FIRST dependencies generation.

CORE_MAKEDEP_CASES = behavior import
CORE_MAKEDEP_TARGETS = $(addprefix core-makedep-,$(CORE_MAKEDEP_CASES))

.PHONY: core-makedep $(CORE_MAKEDEP_TARGETS)

core-makedep: $(CORE_MAKEDEP_TARGETS)

core-makedep-behavior: build clean

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

core-makedep-import: build clean

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
