# Core: Miscellaneous.
#
# The miscellaneous tests use the prefix "core-", not "core-misc-".

CORE_MISC_CASES = clean-crash-dump distclean-tmp help without-edoc without-index without-many
CORE_MISC_TARGETS = $(addprefix core-,$(CORE_MISC_CASES))

.PHONY: core-misc $(CORE_MISC_TARGETS)

core-misc: $(CORE_MISC_TARGETS)

core-clean-crash-dump: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a fake erl_crash.dump file"
	$t touch $(APP)/erl_crash.dump

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that the crash dump is removed"
	$t test ! -e $(APP)/erl_crash.dump

core-distclean-tmp: build clean

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap all $v

	$i "Check that a .erlang.mk directory exists"
	$t test -d $(APP)/.erlang.mk

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that .erlang.mk directory got removed"
	$t test ! -e $(APP)/.erlang.mk

core-help: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Run 'make help' and check that it prints help"
	$t test -n "`$(MAKE) -C $(APP) help` | grep Usage"

core-without-edoc: clean

	$i "Create a working directory for this test"
	$t mkdir -p $(APP)/

	$i "Generate a bleeding edge Erlang.mk without the EDoc plugin"
	$t cd .. && $(MAKE) WITHOUT=plugins/edoc ERLANG_MK=$(CURDIR)/$(APP)/erlang.mk $v

	$i "Confirm that the EDoc plugin was not included."
	$t ! grep -q distclean-edoc $(APP)/erlang.mk

	$i "Update Erlang.mk"
	$t $(MAKE) -C $(APP) -f erlang.mk erlang-mk $v

	$i "Confirm that the EDoc plugin is still not included."
	$t ! grep -q distclean-edoc $(APP)/erlang.mk

core-without-index: clean

	$i "Create a working directory for this test"
	$t mkdir -p $(APP)/

	$i "Generate a bleeding edge Erlang.mk without the package index"
	$t cd .. && $(MAKE) WITHOUT=index ERLANG_MK=$(CURDIR)/$(APP)/erlang.mk $v

	$i "Confirm that the index was not included."
	$t ! grep -q pkg_cowboy $(APP)/erlang.mk

	$i "Update Erlang.mk"
	$t $(MAKE) -C $(APP) -f erlang.mk erlang-mk $v

	$i "Confirm that the index is still not included."
	$t ! grep -q pkg_cowboy $(APP)/erlang.mk

core-without-many: clean

	$i "Create a working directory for this test"
	$t mkdir -p $(APP)/

	$i "Generate a bleeding edge Erlang.mk without the index and the EDoc plugin"
	$t cd .. && $(MAKE) WITHOUT="index plugins/edoc" ERLANG_MK=$(CURDIR)/$(APP)/erlang.mk $v

	$i "Confirm that the EDoc plugin was not included."
	$t ! grep -q distclean-edoc $(APP)/erlang.mk

	$i "Confirm that the index was not included."
	$t ! grep -q pkg_cowboy $(APP)/erlang.mk
