# Core: Erlang.mk upgrade.

CORE_UPGRADE_TARGETS = $(call list_targets,core-upgrade)

.PHONY: core-upgrade $(CORE_UPGRADE_TARGETS)

core-upgrade: $(CORE_UPGRADE_TARGETS)

core-upgrade-changelog: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fork erlang.mk locally and set a test CHANGELOG.asciidoc"
	$t git clone -q https://github.com/ninenines/erlang.mk $(APP)/alt-erlangmk-repo
	$t echo "$(APP)$(APP)" > $(APP)/alt-erlangmk-repo/CHANGELOG.asciidoc
# Since part of this functionality needs the main Makefile, copy it.
	$t cp ../Makefile $(APP)/alt-erlangmk-repo/
	$t (cd $(APP)/alt-erlangmk-repo && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add CHANGELOG.asciidoc Makefile && \
		git commit -q --no-gpg-sign -a -m 'Add test changelog')

	$i "Point application to an alternate erlang.mk repository"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "ERLANG_MK_REPO = file://$(abspath $(APP)/alt-erlangmk-repo)\n"}' $(APP)/Makefile

	$i "Update erlang.mk; CHANGELOG.asciidoc should be printed"
	$t $(MAKE) -C $(APP) erlang-mk | grep -c "$(APP)$(APP)" | grep -q 1

core-upgrade-conflicting-configs: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fork erlang.mk locally and modify it"
	$t git clone -q https://github.com/ninenines/erlang.mk $(APP)/alt-erlangmk-repo
	$t echo core/core > $(APP)/alt-erlangmk-repo/build.config
	$t (cd $(APP)/alt-erlangmk-repo && \
		git checkout -q -b test-modified-build.config && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git commit -q --no-gpg-sign -a -m 'Modify build.config' && \
		git checkout master)

	$i "Point application to an alternate erlang.mk repository"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "ERLANG_MK_REPO = file://$(abspath $(APP)/alt-erlangmk-repo)\nERLANG_MK_COMMIT = test-modified-build.config\n"}' $(APP)/Makefile

	$i "Create a custom build.config file without plugins"
	$t echo "core/*" > $(APP)/build.config

	$i "Upgrade Erlang.mk"
	$t $(MAKE) -C $(APP) erlang-mk $v

	$i "Check that the bootstrap plugin is gone"
	$t ! $(MAKE) -C $(APP) list-templates $v

core-upgrade-custom-build-dir: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Append a rule to the Erlang.mk file for testing purposes"
	$t echo "erlang_mk_upgrade_test_rule: ; @echo FAIL" >> $(APP)/erlang.mk

	$i "Check that the test rule works as intended"
	$t test "FAIL" = "`$(MAKE) -C $(APP) --no-print-directory erlang_mk_upgrade_test_rule V=0`"

	$i "Create the custom build directory"
	$t mkdir $(APP)/custom/
	$t test -d $(APP)/custom/

	$i "Upgrade Erlang.mk with a custom build directory"
	$t ERLANG_MK_BUILD_DIR=custom $(MAKE) -C $(APP) erlang-mk $v

	$i "Check that the rule is gone"
	$t ! $(MAKE) -C $(APP) erlang_mk_upgrade_test_rule $v

	$i "Check that the custom build directory is gone"
	$t test ! -d $(APP)/custom/

core-upgrade-custom-config: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a custom build.config file without plugins"
	$t echo "core/*" > $(APP)/build.config

	$i "Upgrade Erlang.mk"
	$t $(MAKE) -C $(APP) erlang-mk $v

	$i "Check that the bootstrap plugin is gone"
	$t ! $(MAKE) -C $(APP) list-templates $v

core-upgrade-custom-repo: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fork erlang.mk locally and modify it"
	$t git clone -q https://github.com/ninenines/erlang.mk $(APP)/alt-erlangmk-repo
	$t perl -ni.bak -e 'if ($$.==1) {print "# Copyright (c) erlang.mk Testsuite!\n";print}' $(APP)/alt-erlangmk-repo/core/core.mk
	$t (cd $(APP)/alt-erlangmk-repo && \
		git checkout -q -b test-copyright && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git commit -q --no-gpg-sign -a -m 'Add Testsuite copyright' && \
		git checkout master)

	$i "Point application to an alternate erlang.mk repository"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "ERLANG_MK_REPO = file://$(abspath $(APP)/alt-erlangmk-repo)\nERLANG_MK_COMMIT = test-copyright\n"}' $(APP)/Makefile

	$i "Update erlang.mk"
	$t $(MAKE) -C $(APP) erlang-mk $v

	$i "Check our modification is there"
	$t grep -q "# Copyright (c) erlang.mk Testsuite!" $(APP)/erlang.mk

core-upgrade-delete-tmp-dir: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the .erlang.mk directory exists"
	$t test -e $(APP)/.erlang.mk/

	$i "Upgrade Erlang.mk"
	$t $(MAKE) -C $(APP) erlang-mk $v

	$i "Check that the .erlang.mk directory was removed"
	$t ! test -e $(APP)/.erlang.mk/

core-upgrade-no-config: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Append a rule to the Erlang.mk file for testing purposes"
	$t echo "erlang_mk_upgrade_test_rule: ; @echo FAIL" >> $(APP)/erlang.mk

	$i "Check that the test rule works as intended"
	$t test "FAIL" = "`$(MAKE) -C $(APP) --no-print-directory erlang_mk_upgrade_test_rule V=0`"

	$i "Upgrade Erlang.mk"
	$t $(MAKE) -C $(APP) erlang-mk $v

	$i "Check that the rule is gone"
	$t ! $(MAKE) -C $(APP) erlang_mk_upgrade_test_rule $v

core-upgrade-renamed-config: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a custom build.config file without plugins; name it my.build.config"
	$t echo "core/*" > $(APP)/my.build.config

	$i "Set ERLANG_MK_BUILD_CONFIG=my.build.config in the Makefile"
	$t echo "ERLANG_MK_BUILD_CONFIG = my.build.config" >> $(APP)/Makefile

	$i "Upgrade Erlang.mk"
	$t $(MAKE) -C $(APP) erlang-mk $v

	$i "Check that the bootstrap plugin is gone"
	$t ! $(MAKE) -C $(APP) list-templates $v
