# Core: Packages and dependencies.

CORE_DEPS_CASES = pkg search
CORE_DEPS_TARGETS = $(addprefix core-deps-,$(CORE_DEPS_CASES))
CORE_DEPS_CLEAN_TARGETS = $(addprefix clean-,$(CORE_DEPS_TARGETS))

.PHONY: core-deps $(CORE_DEPS_TARGETS) clean-core-deps $(CORE_DEPS_CLEAN_TARGETS)

clean-core-deps: $(CORE_DEPS_CLEAN_TARGETS)

$(CORE_DEPS_CLEAN_TARGETS):
	$t rm -rf $(APP_TO_CLEAN)/

core-deps: $(CORE_DEPS_TARGETS)

core-deps-pkg: build clean-core-deps-pkg

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy to the list of dependencies"
	$t sed -i.bak '2i\
DEPS = cowboy\
' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/cowboy
	$t test -d $(APP)/deps/cowlib
	$t test -d $(APP)/deps/ranch

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), cowboy, cowlib, ranch]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(cowboy, Deps), \
		halt()"

core-deps-search: build clean-core-deps-search

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Run 'make search' and check that it prints packages"
	$t test -n "`$(MAKE) -C $(APP) search`"

	$i "Run 'make search q=cowboy' and check that it prints packages"
	$t test -n "`$(MAKE) -C $(APP) search q=cowboy`"
