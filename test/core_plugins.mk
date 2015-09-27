# Core: External plugins.

CORE_PLUGINS_CASES = all one
CORE_PLUGINS_TARGETS = $(addprefix core-plugins-,$(CORE_PLUGINS_CASES))
CORE_PLUGINS_CLEAN_TARGETS = $(addprefix clean-,$(CORE_PLUGINS_TARGETS))

.PHONY: core-plugins $(CORE_PLUGINS_TARGETS) clean-core-plugins $(CORE_PLUGINS_CLEAN_TARGETS)

clean-core-plugins: $(CORE_PLUGINS_CLEAN_TARGETS)

$(CORE_PLUGINS_CLEAN_TARGETS):
	$t rm -rf $(APP_TO_CLEAN)/

core-plugins: $(CORE_PLUGINS_TARGETS)

core-plugins-all: build clean-core-plugins-all

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
		git commit -q -m "Tests"

	$i "Add dependency and plugins to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = plugin_dep\ndep_plugin_dep = git file://$(abspath $(APP)/plugin_dep) master\nDEP_PLUGINS = plugin_dep\n"}' $(APP)/Makefile

	$i "Run 'make plugin1' and check that it prints plugin1"
	$t test -n "`$(MAKE) -C $(APP) plugin1 | grep plugin1`"

	$i "Run 'make plugin2' and check that it prints plugin2"
	$t test -n "`$(MAKE) -C $(APP) plugin2 | grep plugin2`"

core-plugins-one: build clean-core-plugins-one

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
		git commit -q -m "Tests"

	$i "Add dependency and plugins to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = plugin_dep\ndep_plugin_dep = git file://$(abspath $(APP)/plugin_dep) master\nDEP_PLUGINS = plugin_dep/mk/plugin1.mk\n"}' $(APP)/Makefile

	$i "Run 'make plugin1' and check that it prints plugin1"
	$t test -n "`$(MAKE) -C $(APP) plugin1 | grep plugin1`"

	$i "Run 'make plugin2' and confirm the target doesn't exist"
	$t if `$(MAKE) -C $(APP) plugin2`; then false; fi
