# Core: Templates.

CORE_TEMPLATES_TARGETS = $(call list_targets,core-templates)

.PHONY: core-templates $(CORE_TEMPLATES_TARGETS)

core-templates: $(CORE_TEMPLATES_TARGETS)

core-templates-hrl: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create test.hrl"
	$t $(MAKE) -C $(APP) -f erlang.mk new t=hrl n=test
	$t test -f $(APP)/include/test.hrl