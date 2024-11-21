# Core: External plugins.

core_plugins_TARGETS = $(call list_targets,core-plugins)

.PHONY: core-plugins $(core_plugins_TARGETS)

core-plugins: $(core_plugins_TARGETS)

core-plugins-all: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a local git repository with two plugins"
	$t mkdir -p $(APP)/plugin_dep/mk
	$t echo "plugin1: ; @echo \$$@" > $(APP)/plugin_dep/mk/plugin1.mk
	$t echo "plugin2: ; @echo \$$@" > $(APP)/plugin_dep/mk/plugin2.mk
	$t echo "THIS := \$$(dir \$$(realpath \$$(lastword \$$(MAKEFILE_LIST))))" > $(APP)/plugin_dep/plugins.mk
	$t printf "%s\n" "include \$$(THIS)/mk/plugin1.mk" >> $(APP)/plugin_dep/plugins.mk
	$t printf "%s\n" "include \$$(THIS)/mk/plugin2.mk" >> $(APP)/plugin_dep/plugins.mk
# We check that overriding THIS doesn't cause an error.
	$t echo "THIS :=" >> $(APP)/plugin_dep/plugins.mk
	$t cd $(APP)/plugin_dep && \
		git init -q && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add dependency and plugins to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = plugin_dep\ndep_plugin_dep = git file://$(abspath $(APP)/plugin_dep) master\nDEP_PLUGINS = plugin_dep\n"}' $(APP)/Makefile

	$i "Run 'make plugin1' and check that it prints plugin1"
	$t test -n "`$(MAKE) -C $(APP) plugin1 | grep plugin1`"

	$i "Run 'make plugin2' and check that it prints plugin2"
	$t test -n "`$(MAKE) -C $(APP) plugin2 | grep plugin2`"

core-plugins-early: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Write external plugin adddep_plugin"
	$t mkdir $(APP)/adddep_plugin
	$t echo "DEPS += cowlib" >> $(APP)/adddep_plugin/early-plugins.mk

	$i "Inject external plugin dependencies into $(APP)"
	$t echo 'DEPS = ranch' >> $(APP)/Makefile.tmp
	$t echo 'BUILD_DEPS = adddep_plugin' >> $(APP)/Makefile.tmp
	$t echo 'DEP_EARLY_PLUGINS = adddep_plugin' >> $(APP)/Makefile.tmp
	$t echo 'dep_adddep_plugin = cp adddep_plugin' >> $(APP)/Makefile.tmp
	$t cat $(APP)/Makefile >> $(APP)/Makefile.tmp
	$t mv $(APP)/Makefile.tmp $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -e $(APP)/deps/cowlib
	$t test -e $(APP)/deps/ranch

core-plugins-early-local: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create two internal plugin makefiles"
	$t mkdir -p $(APP)/mk
	$t echo "plugin1: ; @echo \$$@" > $(APP)/mk/plugin1.mk
	$t echo "plugin2: ; @echo \$$@" > $(APP)/early-plugins.mk

	$i "Add dependency and plugins to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEP_EARLY_PLUGINS = \$$(PROJECT) \$$(PROJECT)/mk/plugin1.mk\n"}' $(APP)/Makefile

	$i "Run 'make plugin1' and check that it prints plugin1"
	$t $(MAKE) --no-print-directory -C $(APP) plugin1 | grep -qw plugin1

	$i "Run 'make plugin2' and check that it prints plugin2"
	$t $(MAKE) --no-print-directory -C $(APP) plugin2 | grep -qw plugin2

core-plugins-early-help: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Write external plugin helpful_plugin"
	$t mkdir $(APP)/helpful_plugin
	$t echo "help-plugins:: ; @echo WORKING" >> $(APP)/helpful_plugin/early-plugins.mk

	$i "Inject external plugin dependencies into $(APP)"
	$t echo 'BUILD_DEPS = helpful_plugin' >> $(APP)/Makefile.tmp
	$t echo 'DEP_EARLY_PLUGINS = helpful_plugin' >> $(APP)/Makefile.tmp
	$t echo 'dep_helpful_plugin = cp helpful_plugin' >> $(APP)/Makefile.tmp
	$t cat $(APP)/Makefile >> $(APP)/Makefile.tmp
	$t mv $(APP)/Makefile.tmp $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Run 'make help' and check that it prints external plugins help"
	$t test -n "`$(MAKE) -C $(APP) help` | grep WORKING"

core-plugins-early-rebar: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Write external plugin rebar_plugin"
	$t mkdir $(APP)/rebar_plugin
	$t echo "rebar:: ; touch rebar.config" >> $(APP)/rebar_plugin/early-plugins.mk

	$i "Inject external plugin dependencies into $(APP)"
	$t echo 'BUILD_DEPS = rebar_plugin' >> $(APP)/Makefile.tmp
	$t echo 'DEP_EARLY_PLUGINS = rebar_plugin' >> $(APP)/Makefile.tmp
	$t echo 'dep_rebar_plugin = cp rebar_plugin' >> $(APP)/Makefile.tmp
	$t cat $(APP)/Makefile >> $(APP)/Makefile.tmp
	$t mv $(APP)/Makefile.tmp $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

core-plugins-local: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create two internal plugin makefiles"
	$t mkdir -p $(APP)/mk
	$t echo "plugin1: ; @echo \$$@" > $(APP)/mk/plugin1.mk
	$t echo "plugin2: ; @echo \$$@" > $(APP)/plugins.mk

	$i "Add dependency and plugins to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEP_PLUGINS = \$$(PROJECT) \$$(PROJECT)/mk/plugin1.mk\n"}' $(APP)/Makefile

	$i "Run 'make plugin1' and check that it prints plugin1"
	$t $(MAKE) --no-print-directory -C $(APP) plugin1 | grep -qw plugin1

	$i "Run 'make plugin2' and check that it prints plugin2"
	$t $(MAKE) --no-print-directory -C $(APP) plugin2 | grep -qw plugin2

core-plugins-one: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a local git repository with two plugins"
	$t mkdir -p $(APP)/plugin_dep/mk
	$t echo "plugin1: ; @echo \$$@" > $(APP)/plugin_dep/mk/plugin1.mk
	$t echo "plugin2: ; @echo \$$@" > $(APP)/plugin_dep/mk/plugin2.mk
	$t echo "THIS := \$$(dir \$$(realpath \$$(lastword \$$(MAKEFILE_LIST))))" > $(APP)/plugin_dep/plugins.mk
	$t printf "%s\n" "include \$$(THIS)/mk/plugin1.mk" >> $(APP)/plugin_dep/plugins.mk
	$t printf "%s\n" "include \$$(THIS)/mk/plugin2.mk" >> $(APP)/plugin_dep/plugins.mk
# We check that overriding THIS doesn't cause an error.
	$t echo "THIS :=" >> $(APP)/plugin_dep/plugins.mk
	$t cd $(APP)/plugin_dep && \
		git init -q && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add dependency and plugins to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = plugin_dep\ndep_plugin_dep = git file://$(abspath $(APP)/plugin_dep) master\nDEP_PLUGINS = plugin_dep/mk/plugin1.mk\n"}' $(APP)/Makefile

	$i "Run 'make plugin1' and check that it prints plugin1"
	$t $(MAKE) --no-print-directory -C $(APP) plugin1 | grep -qw plugin1

	$i "Run 'make plugin2' and confirm the target doesn't exist"
	$t ! $(MAKE) --no-print-directory -C $(APP) plugin2

core-plugins-templates: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a local git repository with a plugin containing a template"
	$t mkdir -p $(APP)/plugin_dep
	$t printf "%s\n" \
		"define tpl_test_mk" \
		"-module(template_name)." \
		"endef" > $(APP)/plugin_dep/plugins.mk
	$t cd $(APP)/plugin_dep && \
		git init -q && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add dependency and plugins to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = plugin_dep\ndep_plugin_dep = git file://$(abspath $(APP)/plugin_dep) master\nDEP_PLUGINS = plugin_dep\n"}' $(APP)/Makefile

	$i "Run 'make list-templates' and check that it prints test_mk"
	$t $(MAKE) --no-print-directory -C $(APP) list-templates | grep -qw test_mk

	$i "Create a new file using the template"
	$t $(MAKE) --no-print-directory -C $(APP) new t=test_mk n=test_mk $v

	$i "Confirm the file exists"
	$t test -f $(APP)/src/test_mk.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the file was compiled correctly"
	$t test -f $(APP)/ebin/test_mk.beam

core-plugins-templates-apps-only: init

	$i "Create a multi application repository with no root application"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t echo "include erlang.mk" > $(APP)/Makefile

	$i "Create a new application my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Create a local git repository with a plugin containing a template"
	$t mkdir -p $(APP)/plugin_dep
	$t printf "%s\n" \
		"define tpl_test_mk" \
		"-module(test_mk)." \
		"endef" > $(APP)/plugin_dep/plugins.mk
	$t cd $(APP)/plugin_dep && \
		git init -q && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add dependency and plugins to the Makefile"
	$t printf "%s\n" \
		"DEPS = plugin_dep" \
		"dep_plugin_dep = git file://$(abspath $(APP)/plugin_dep) master" \
		"DEP_PLUGINS = plugin_dep" \
		"include erlang.mk" > $(APP)/Makefile

	$i "Create a new file using the template in the my_app application"
	$t $(MAKE) --no-print-directory -C $(APP) new t=test_mk n=test_mk in=my_app $v

	$i "Confirm the file exists"
	$t test -f $(APP)/apps/my_app/src/test_mk.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the file was compiled correctly"
	$t test -f $(APP)/apps/my_app/ebin/test_mk.beam

core-plugins-templates-file: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a local git repository with a plugin containing a template file"
	$t mkdir -p $(APP)/plugin_dep/templates
	$t printf "%s\n" "-module(template_name)." > $(APP)/plugin_dep/templates/test_mk.erl
	$t echo "THIS := \$$(dir \$$(realpath \$$(lastword \$$(MAKEFILE_LIST))))" > $(APP)/plugin_dep/plugins.mk
	$t printf "%s\n" "tpl_test_mk = \$$(file < \$$(THIS)/templates/test_mk.erl)" >> $(APP)/plugin_dep/plugins.mk
	$t cd $(APP)/plugin_dep && \
		git init -q && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add dependency and plugins to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = plugin_dep\ndep_plugin_dep = git file://$(abspath $(APP)/plugin_dep) master\nDEP_PLUGINS = plugin_dep\n"}' $(APP)/Makefile

	$i "Run 'make list-templates' and check that it prints test_mk"
	$t $(MAKE) --no-print-directory -C $(APP) list-templates | grep -qw test_mk

	$i "Create a new file using the template"
	$t $(MAKE) --no-print-directory -C $(APP) new t=test_mk n=test_mk $v

	$i "Confirm the file exists"
	$t test -f $(APP)/src/test_mk.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the file was compiled correctly"
	$t test -f $(APP)/ebin/test_mk.beam

core-plugins-test: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Write external plugin touch_plugin"
	$t mkdir $(APP)/touch_plugin
	$t printf "%s\n" \
		"app::" \
		"	touch markerfile" \
		"test-build:: app" \
		"clean::" \
		"	rm -f markerfile" \
		> $(APP)/touch_plugin/plugins.mk

	$i "Inject external plugin dependencies into $(APP)"
	$t echo 'BUILD_DEPS = touch_plugin' >> $(APP)/Makefile.tmp
	$t echo 'DEP_PLUGINS = touch_plugin' >> $(APP)/Makefile.tmp
	$t echo 'dep_touch_plugin = cp touch_plugin' >> $(APP)/Makefile.tmp
	$t cat $(APP)/Makefile >> $(APP)/Makefile.tmp
	$t mv $(APP)/Makefile.tmp $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the application was compiled correctly"
	$t test -e $(APP)/markerfile

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that the application was cleaned correctly"
	$t test ! -e $(APP)/markerfile

	$i "Run tests"
	$t $(MAKE) -C $(APP) tests $v

	$i "Check that the application was compiled correctly"
	$t test -e $(APP)/markerfile
