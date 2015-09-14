# Core: Packages and dependencies.

CORE_DEPS_CASES = apps apps-build-count apps-conflict apps-deep-conflict apps-dir apps-dir-include-lib apps-new-app apps-new-lib apps-new-tpl apps-only autopatch-rebar build-c-8cc build-c-imagejs build-erl build-js dep-commit dir doc fetch-cp fetch-custom fetch-fail-bad fetch-fail-unknown fetch-git fetch-git-submodule fetch-hex fetch-hg fetch-legacy fetch-svn ignore list-deps mv mv-rebar no-autopatch no-autopatch-erlang-mk no-autopatch-rebar order-first order-top otp pkg rel search shell skip test
CORE_DEPS_TARGETS = $(addprefix core-deps-,$(CORE_DEPS_CASES))

.PHONY: core-deps $(CORE_DEPS_TARGETS)

core-deps: $(CORE_DEPS_TARGETS)

core-deps-apps: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

# Bootstrap the application manually to make sure it works as intended.
	$i "Bootstrap a repository-local application my_app"
	$t mkdir -p $(APP)/apps/my_app/src/
	$t touch $(APP)/apps/file.erl
	$t echo "DEPS = cowlib" > $(APP)/apps/my_app/Makefile
	$t echo "include ../../erlang.mk" >> $(APP)/apps/my_app/Makefile
	$t echo "-module(boy)." > $(APP)/apps/my_app/src/boy.erl
	$t echo "-module(girl)." > $(APP)/apps/my_app/src/girl.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/apps/my_app/my_app.d
	$t test -f $(APP)/apps/my_app/ebin/my_app.app
	$t test -f $(APP)/apps/my_app/ebin/boy.beam
	$t test -f $(APP)/apps/my_app/ebin/girl.beam
	$t test -d $(APP)/deps/cowlib

# Applications in apps are compiled automatically but not added
# to the application resource file unless they are listed in LOCAL_DEPS.
	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/apps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), my_app]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(my_app, Deps), \
		{ok, MyAppDeps} = application:get_key(my_app, applications), \
		true = lists:member(cowlib, MyAppDeps), \
		{ok, []} = application:get_key($(APP), modules), \
		{ok, Mods = [boy, girl]} = application:get_key(my_app, modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that Cowlib is still here"
	$t test -d $(APP)/deps/cowlib

	$i "Check that all relevant files were removed"
	$t test ! -e $(APP)/$(APP).d
	$t test ! -e $(APP)/ebin/$(APP).app
	$t test ! -e $(APP)/apps/my_app/my_app.d
	$t test ! -e $(APP)/apps/my_app/ebin/my_app.app
	$t test ! -e $(APP)/apps/my_app/ebin/boy.beam
	$t test ! -e $(APP)/apps/my_app/ebin/girl.beam

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that all relevant files were removed"
	$t test ! -e $(APP)/deps

	$i "Add my_app to the local dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = my_app\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add my_app to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tmy_app,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/apps/my_app/my_app.d
	$t test -f $(APP)/apps/my_app/ebin/my_app.app
	$t test -f $(APP)/apps/my_app/ebin/boy.beam
	$t test -f $(APP)/apps/my_app/ebin/girl.beam
	$t test -d $(APP)/deps/cowlib

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/apps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), my_app]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(my_app, Deps), \
		{ok, MyAppDeps} = application:get_key(my_app, applications), \
		true = lists:member(cowlib, MyAppDeps), \
		{ok, []} = application:get_key($(APP), modules), \
		{ok, Mods = [boy, girl]} = application:get_key(my_app, modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"

core-deps-apps-build-count: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$t mkdir -p "$(APP)/priv/dep"
	$t echo "PROJECT = fake_dep" > $(APP)/priv/dep/Makefile
	$t echo "include ../../erlang.mk" >> $(APP)/priv/dep/Makefile

	$i "Bootstrap a repository-local application my_app"
	$t echo "DEPS = dep1 dep2 dep3 dep4" > $(APP)/Makefile
	$t echo "dep_dep1 = cp ./priv/dep" >> $(APP)/Makefile
	$t echo "dep_dep2 = cp ./priv/dep" >> $(APP)/Makefile
	$t echo "dep_dep3 = cp ./priv/dep" >> $(APP)/Makefile
	$t echo "dep_dep4 = cp ./priv/dep" >> $(APP)/Makefile
	$t echo "include erlang.mk" >> $(APP)/Makefile

	$i "Create a new application app_one"
	$t $(MAKE) -C $(APP) new-app in=app_one $v
	$t echo "all::" >> $(APP)/apps/app_one/Makefile
	$t echo "	@echo -n '#' >> count" >> $(APP)/apps/app_one/Makefile

	$i "Create a new application app_two"
	$t $(MAKE) -C $(APP) new-app in=app_two $v
	$t echo "all::" >> $(APP)/apps/app_two/Makefile
	$t echo "	@echo -n '#' >> count" >> $(APP)/apps/app_two/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check the number of times each app was compiled"
	$t test "`wc -c $(APP)/apps/app_one/count | awk '{printf $$1}'`" -eq 1
	$t test "`wc -c $(APP)/apps/app_two/count | awk '{printf $$1}'`" -eq 1

core-deps-apps-conflict: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/Makefile

	$i "Create a new library Cowlib"
	$t $(MAKE) -C $(APP) new-lib in=cowlib $v

	$i "Check that building the application fails because of a conflict"
	$t ! $(MAKE) -C $(APP) $v

	$i "Check that Cowlib wasn't fetched"
	$t test ! -e $(APP)/deps/cowlib

core-deps-apps-deep-conflict: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\n"}' $(APP)/Makefile

	$i "Create a new library Cowlib"
	$t $(MAKE) -C $(APP) new-lib in=cowlib $v

	$i "Check that building the application fails because of a conflict"
	$t ! $(MAKE) -C $(APP) $v

	$i "Check that Cowlib wasn't fetched"
	$t test ! -e $(APP)/deps/cowlib

core-deps-apps-dir: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Set a custom APPS_DIR"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "APPS_DIR ?= \$$(CURDIR)/deep/libs\n"}' $(APP)/Makefile

	$i "Create a new library my_app"
	$t $(MAKE) -C $(APP) new-lib in=my_app $v

	$i "Add Cowlib as a dependency to my_app"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/deep/libs/my_app/Makefile

ifdef LEGACY
	$i "Add Cowlib to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowlib,\n"}' $(APP)/deep/libs/my_app/src/my_app.app.src
endif

	$i "Generate .erl files in my_app"
	$t echo "-module(boy)." > $(APP)/deep/libs/my_app/src/boy.erl
	$t echo "-module(girl)." > $(APP)/deep/libs/my_app/src/girl.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/deep/libs/my_app/my_app.d
	$t test -f $(APP)/deep/libs/my_app/ebin/my_app.app
	$t test -f $(APP)/deep/libs/my_app/ebin/boy.beam
	$t test -f $(APP)/deep/libs/my_app/ebin/girl.beam
	$t test -d $(APP)/deps/cowlib

# Applications in apps are compiled automatically but not added
# to the application resource file unless they are listed in LOCAL_DEPS.
	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deep/libs/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), my_app]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(my_app, Deps), \
		{ok, MyAppDeps} = application:get_key(my_app, applications), \
		true = lists:member(cowlib, MyAppDeps), \
		{ok, []} = application:get_key($(APP), modules), \
		{ok, Mods = [boy, girl]} = application:get_key(my_app, modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that Cowlib is still here"
	$t test -d $(APP)/deps/cowlib

	$i "Check that all relevant files were removed"
	$t test ! -e $(APP)/$(APP).d
	$t test ! -e $(APP)/ebin/$(APP).app
	$t test ! -e $(APP)/libs/my_app/my_app.d
	$t test ! -e $(APP)/libs/my_app/ebin/my_app.app
	$t test ! -e $(APP)/libs/my_app/ebin/boy.beam
	$t test ! -e $(APP)/libs/my_app/ebin/girl.beam

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that all relevant files were removed"
	$t test ! -e $(APP)/deps

core-deps-apps-dir-include-lib: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Set a custom APPS_DIR"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "APPS_DIR ?= \$$(CURDIR)/deep/libs\n"}' $(APP)/Makefile

	$i "Create new libraries boy_app and girl_app"
	$t $(MAKE) -C $(APP) new-lib in=boy_app $v
	$t $(MAKE) -C $(APP) new-lib in=girl_app $v

	$i "Generate .erl and .hrl files in apps with mutual include_lib()"
	$t echo '-module(boy).  -include_lib("girl_app/include/girl.hrl").' > $(APP)/deep/libs/boy_app/src/boy.erl
	$t echo '-module(girl). -include_lib("boy_app/include/boy.hrl").' > $(APP)/deep/libs/girl_app/src/girl.erl
	$t mkdir -p $(APP)/deep/libs/boy_app/include
	$t echo '%% boy'  > $(APP)/deep/libs/boy_app/include/boy.hrl
	$t mkdir -p $(APP)/deep/libs/girl_app/include
	$t echo '%% girl' > $(APP)/deep/libs/girl_app/include/girl.hrl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app

	$t test -f $(APP)/deep/libs/boy_app/boy_app.d
	$t test -f $(APP)/deep/libs/boy_app/ebin/boy_app.app
	$t test -f $(APP)/deep/libs/boy_app/ebin/boy.beam

	$t test -f $(APP)/deep/libs/girl_app/girl_app.d
	$t test -f $(APP)/deep/libs/girl_app/ebin/girl_app.app
	$t test -f $(APP)/deep/libs/girl_app/ebin/girl.beam

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

core-deps-apps-new-app: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a new application my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/apps/my_app/Makefile
ifdef LEGACY
	$t test -f $(APP)/apps/my_app/src/my_app.app.src
endif
	$t test -f $(APP)/apps/my_app/src/my_app_app.erl
	$t test -f $(APP)/apps/my_app/src/my_app_sup.erl

	$i "Create a new module my_server in my_app"
	$t $(MAKE) -C $(APP) new t=gen_server n=my_server in=my_app $v

	$i "Check that the file exists"
	$t test -f $(APP)/apps/my_app/src/my_server.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/apps/my_app/my_app.d
	$t test -f $(APP)/apps/my_app/ebin/my_app.app
	$t test -f $(APP)/apps/my_app/ebin/my_app_app.beam
	$t test -f $(APP)/apps/my_app/ebin/my_app_sup.beam
	$t test -f $(APP)/apps/my_app/ebin/my_server.beam

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/apps/*/ebin/ -eval " \
		ok = application:start(my_app), \
		{ok, [my_app_app, my_app_sup, my_server]} = application:get_key(my_app, modules), \
		{module, my_app_app} = code:load_file(my_app_app), \
		{module, my_app_sup} = code:load_file(my_app_sup), \
		{module, my_server} = code:load_file(my_server), \
		halt()"

core-deps-apps-new-lib: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a new application my_app"
	$t $(MAKE) -C $(APP) new-lib in=my_app $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/apps/my_app/Makefile
ifdef LEGACY
	$t test -f $(APP)/apps/my_app/src/my_app.app.src
endif

	$i "Create a new module my_server in my_app"
	$t $(MAKE) -C $(APP) new t=gen_server n=my_server in=my_app $v

	$i "Check that the file exists"
	$t test -f $(APP)/apps/my_app/src/my_server.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/apps/my_app/my_app.d
	$t test -f $(APP)/apps/my_app/ebin/my_app.app
	$t test -f $(APP)/apps/my_app/ebin/my_server.beam

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/apps/*/ebin/ -eval " \
		ok = application:start(my_app), \
		{ok, [my_server]} = application:get_key(my_app, modules), \
		{module, my_server} = code:load_file(my_server), \
		halt()"

core-deps-apps-new-tpl: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a new library my_app"
	$t $(MAKE) -C $(APP) new-lib in=my_app $v

	$i "Generate one of each template"
	$t $(MAKE) -C $(APP) --no-print-directory new in=my_app t=gen_fsm n=my_fsm
	$t $(MAKE) -C $(APP) --no-print-directory new in=my_app t=gen_server n=my_server
	$t $(MAKE) -C $(APP) --no-print-directory new in=my_app t=supervisor n=my_sup

# Here we disable warnings because templates contain missing behaviors.
	$i "Build the application"
	$t $(MAKE) -C $(APP)/apps/my_app ERLC_OPTS=+debug_info $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/apps/my_app/ebin/my_app.app
	$t test -f $(APP)/apps/my_app/ebin/my_fsm.beam
	$t test -f $(APP)/apps/my_app/ebin/my_server.beam
	$t test -f $(APP)/apps/my_app/ebin/my_sup.beam

	$i "Check that all the modules can be loaded"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/apps/*/ebin/ -eval " \
		ok = application:start(my_app), \
		{ok, Mods = [my_fsm, my_server, my_sup]} = application:get_key(my_app, modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"

core-deps-apps-only: build clean

	$i "Create a multi application repository with no root application"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t echo "include erlang.mk" > $(APP)/Makefile

	$i "Create a new application my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Create a module my_server from gen_server template in my_app"
	$t $(MAKE) -C $(APP) new t=gen_server n=my_server in=my_app $v

	$i "Add Cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/apps/my_app/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/apps/my_app/my_app.d
	$t test -f $(APP)/apps/my_app/ebin/my_app.app
	$t test -f $(APP)/apps/my_app/ebin/my_app_app.beam
	$t test -f $(APP)/apps/my_app/ebin/my_app_sup.beam
	$t test -f $(APP)/apps/my_app/ebin/my_server.beam
	$t test -d $(APP)/deps/cowlib/

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/apps/*/ebin/ -eval " \
		ok = application:load(my_app), \
		{ok, Mods = [my_app_app, my_app_sup, my_server]} = application:get_key(my_app, modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that Cowlib is still here"
	$t test -d $(APP)/deps/cowlib

	$i "Check that all relevant files were removed"
	$t test ! -e $(APP)/apps/my_app/my_app.d
	$t test ! -e $(APP)/apps/my_app/ebin/my_app.app
	$t test ! -e $(APP)/apps/my_app/ebin/my_app_app.beam
	$t test ! -e $(APP)/apps/my_app/ebin/my_app_sup.beam
	$t test ! -e $(APP)/apps/my_app/ebin/my_server.beam

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that all relevant files were removed"
	$t test ! -e $(APP)/deps

core-deps-autopatch-rebar: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add erlsha2 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = erlsha2\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that erlsha2 was fetched and built"
	$t test -d $(APP)/deps/erlsha2
	$t test -f $(APP)/deps/erlsha2/ebin/erlsha2.beam
ifneq ($(PLATFORM),msys2)
	$t test -f $(APP)/deps/erlsha2/priv/erlsha2_nif.so
endif

ifneq ($(PLATFORM),msys2)
core-deps-build-c-8cc: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add 8cc to the list of build dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = 8cc\ndep_8cc = git https://github.com/rui314/8cc master\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/8cc

	$i "Check that 8cc can be started"
	$t $(APP)/deps/8cc/8cc -h $v

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member('8cc', Deps), \
		halt()"
endif

ifneq ($(PLATFORM),freebsd)
core-deps-build-c-imagejs: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add imagejs to the list of build dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = imagejs\ndep_imagejs = git https://github.com/jklmnn/imagejs master\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/imagejs

	$i "Check that imagejs works"
	$t $(APP)/deps/imagejs/imagejs bmp $(APP)/deps/imagejs/Makefile
	$t test -f $(APP)/deps/imagejs/Makefile.bmp

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(imagejs, Deps), \
		halt()"
endif

core-deps-build-erl: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add cowlib to the list of build dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = cowlib\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/cowlib

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), cowlib]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(cowlib, Deps), \
		halt()"

core-deps-build-js: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add jquery to the list of build dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = jquery\ndep_jquery = git https://github.com/jquery/jquery master\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/jquery

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(jquery, Deps), \
		halt()"

core-deps-dep-commit: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 1.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy_commit = 1.0.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

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
		{ok, \"1.0.0\"} = application:get_key(cowboy, vsn), \
		halt()"

core-deps-dir: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy to the list of dependencies with a custom DEPS_DIR"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\nDEPS_DIR ?= \$$(CURDIR)/deep/libs\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched in the custom DEPS_DIR"
	$t test -d $(APP)/deep/libs/cowboy
	$t test -d $(APP)/deep/libs/cowlib
	$t test -d $(APP)/deep/libs/ranch

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deep/libs/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), cowboy, cowlib, ranch]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(cowboy, Deps), \
		halt()"

core-deps-doc: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate .erl files"
	$t echo "-module(boy)." > $(APP)/src/boy.erl
	$t echo "-module(girl)." > $(APP)/src/girl.erl

	$i "Add Edown as a documentation building dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DOC_DEPS = edown\nEDOC_OPTS = {doclet, edown_doclet}\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that no dependencies were fetched"
	$t test ! -e $(APP)/deps

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(edown, Deps), \
		halt()"

	$i "Build the application documentation"
	$t $(MAKE) -C $(APP) edoc $v

	$i "Check that documentation dependencies were fetched"
	$t test -d $(APP)/deps/edown

	$i "Check the Edown generated Markdown documentation"
	$t test -f $(APP)/doc/boy.md
	$t test -f $(APP)/doc/girl.md

core-deps-fetch-cp: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Bootstrap a new OTP library named my_dep inside $(APP)"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap-lib $v

	$i "Add my_dep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = cp $(CURDIR)/$(APP)/my_dep/\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add my_dep to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tmy_dep,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/my_dep

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), my_dep]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(my_dep, Deps), \
		halt()"

core-deps-fetch-custom: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add dependency boop using custom fetch method beep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = boop\ndep_boop = beep boop\ndep_fetch_beep = mkdir -p \$$(DEPS_DIR)/\$$1/ebin/\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add boop to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tboop,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/boop

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(boop, Deps), \
		halt()"

core-deps-fetch-fail-bad: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib as a dependency using a non-existing fetch method named oops"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = oops https://github.com/ninenines/cowlib 1.0.0\n"}' $(APP)/Makefile

	$i "Check that building the application fails"
	$t ! $(MAKE) -C $(APP) $v

core-deps-fetch-fail-unknown: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add an unknown application as a dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = unknown\n"}' $(APP)/Makefile

	$i "Check that building the application fails"
	$t ! $(MAKE) -C $(APP) $v

core-deps-fetch-git: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 1.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy = git https://github.com/ninenines/cowboy 1.0.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

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
		{ok, \"1.0.0\"} = application:get_key(cowboy, vsn), \
		halt()"

core-deps-fetch-git-submodule: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Bootstrap a new OTP library named my_dep inside $(APP)"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap-lib $v
# Create an empty file so src/ gets committed.
	$t touch $(APP)/my_dep/src/README
	$t cd $(APP)/my_dep && \
		git init -q && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q -m "Tests"

	$i "Add the submodule to my_dep"
	$t mkdir $(APP)/deps
	$t cd $(APP) && \
		git init -q && \
		git submodule -q add file://$(abspath $(APP)/my_dep) deps/my_dep && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q -m "Tests"

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Add my_dep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git-submodule\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add my_dep to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tmy_dep,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/my_dep

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), my_dep]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(my_dep, Deps), \
		halt()"

core-deps-fetch-hex: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 1.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy = hex 1.0.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

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
		{ok, \"1.0.0\"} = application:get_key(cowboy, vsn), \
		halt()"

core-deps-fetch-hg: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Ehsa 4.0.3 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = ehsa\ndep_ehsa = hg https://bitbucket.org/a12n/ehsa 4.0.3\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add ehsa to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tehsa,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/ehsa

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), ehsa]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(ehsa, Deps), \
		{ok, \"4.0.3\"} = application:get_key(ehsa, vsn), \
		halt()"

# Legacy must fail for the top-level application, but work for dependencies.
core-deps-fetch-legacy: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib as a dependency using a non-existing fetch method named oops"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = https://github.com/ninenines/cowlib 1.0.0\n"}' $(APP)/Makefile

	$i "Check that building the application fails"
	$t ! $(MAKE) -C $(APP) $v

	$i "Check that building the application works with IS_DEP=1"
	$t $(MAKE) -C $(APP) IS_DEP=1 $v

core-deps-fetch-svn: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib 1.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = svn https://github.com/ninenines/cowlib/tags/1.0.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowlib to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowlib,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/cowlib

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), cowlib]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(cowlib, Deps), \
		{ok, \"1.0.0\"} = application:get_key(cowlib, vsn), \
		halt()"

core-deps-ignore: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy to dependencies, Ranch to the ignore list and to test dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\nIGNORE_DEPS = ranch\nTEST_DEPS = ranch\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the correct dependencies were fetched"
	$t test -d $(APP)/deps/cowboy
	$t test -d $(APP)/deps/cowlib
	$t test ! -e $(APP)/deps/ranch

	$i "Build the test dependencies"
	$t $(MAKE) -C $(APP) test-deps $v

	$i "Check that the correct dependencies were fetched"
	$t test -d $(APP)/deps/ranch

define add_dep_and_subdep
	$i "Bootstrap a new OTP library named $(APP)-$(1)subdep"
	$t mkdir $(APP)-$(1)subdep/
	$t cp ../erlang.mk $(APP)-$(1)subdep/
	$t $(MAKE) -C $(APP)-$(1)subdep --no-print-directory -f erlang.mk bootstrap-lib $$v

	$i "Create a Git repository for $(APP)-$(1)subdep"
	$t (cd $(APP)-$(1)subdep && \
		git init -q && \
		git config user.name "Testsuite" && \
		git config user.email "testsuite@erlang.mk" && \
		git add . && \
		git commit -q -m "Initial commit")

	$i "Bootstrap a new OTP library named $(APP)-$(1)dep"
	$t mkdir $(APP)-$(1)dep/
	$t cp ../erlang.mk $(APP)-$(1)dep/
	$t $(MAKE) -C $(APP)-$(1)dep --no-print-directory -f erlang.mk bootstrap-lib $$v

	$i "Add $(APP)-$(1)subdep as a dependency"
	$t perl -ni.bak -e \
		'print;if ($$.==1) {print "DEPS = $(1)subdep\ndep_$(1)subdep = git file://$(abspath $(APP)-$(1)subdep) master\n"}' \
		$(APP)-$(1)dep/Makefile
	$t rm $(APP)-$(1)dep/Makefile.bak

	$i "Create a Git repository for $(APP)-$(1)dep"
	$t (cd $(APP)-$(1)dep && \
		git init -q && \
		git config user.name "Testsuite" && \
		git config user.email "testsuite@erlang.mk" && \
		git add . && \
		git commit -q -m "Initial commit")
endef

core-deps-list-deps: build clean

	$(call add_dep_and_subdep,)
	$(call add_dep_and_subdep,doc)
	$(call add_dep_and_subdep,rel)
	$(call add_dep_and_subdep,test)
	$(call add_dep_and_subdep,shell)

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add $(APP)-dep and $(APP)-testdep as a dependency"
	$t sed -i.bak '2i\
DEPS = dep\
DOC_DEPS = docdep\
REL_DEPS = reldep\
TEST_DEPS = testdep\
SHELL_DEPS = shelldep\
dep_dep = git file://$(abspath $(APP)-dep) master\
dep_docdep = git file://$(abspath $(APP)-docdep) master\
dep_reldep = git file://$(abspath $(APP)-reldep) master\
dep_testdep = git file://$(abspath $(APP)-testdep) master\
dep_shelldep = git file://$(abspath $(APP)-shelldep) master\
' $(APP)/Makefile
	$t rm $(APP)/Makefile.bak

	$i "Create a Git repository for $(APP)"
	$t (cd $(APP) && \
		git init -q && \
		git config user.name "Testsuite" && \
		git config user.email "testsuite@erlang.mk" && \
		git add . && \
		git commit -q -m "Initial commit")

	$i "List application dependencies"
	$t $(MAKE) -C $(APP) --no-print-directory list-deps $v
	$t test -d $(APP)/deps/subdep
	$t printf "%s\n%s\n" $(abspath $(APP)/deps/dep $(APP)/deps/subdep) > $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/recursive-deps-list.log
	$t $(MAKE) -C $(APP) --no-print-directory distclean $v

	$i "List application doc dependencies"
	$t $(MAKE) -C $(APP) --no-print-directory list-doc-deps $v
	$t test -d $(APP)/deps/subdep
	$t test -d $(APP)/deps/docsubdep
	$t printf "%s\n%s\n%s\n%s\n" \
		$(abspath $(APP)/deps/dep $(APP)/deps/subdep $(APP)/deps/docdep $(APP)/deps/docsubdep) \
		| sort > $(APP)/expected-doc-deps.txt
	$t cmp $(APP)/expected-doc-deps.txt $(APP)/.erlang.mk/recursive-doc-deps-list.log
	$t $(MAKE) -C $(APP) --no-print-directory distclean $v

	$i "List application rel dependencies"
	$t $(MAKE) -C $(APP) --no-print-directory list-rel-deps $v
	$t test -d $(APP)/deps/subdep
	$t test -d $(APP)/deps/relsubdep
	$t printf "%s\n%s\n%s\n%s\n" \
		$(abspath $(APP)/deps/dep $(APP)/deps/subdep $(APP)/deps/reldep $(APP)/deps/relsubdep) \
		| sort > $(APP)/expected-rel-deps.txt
	$t cmp $(APP)/expected-rel-deps.txt $(APP)/.erlang.mk/recursive-rel-deps-list.log
	$t $(MAKE) -C $(APP) --no-print-directory distclean $v

	$i "List application test dependencies"
	$t $(MAKE) -C $(APP) --no-print-directory list-test-deps $v
	$t test -d $(APP)/deps/subdep
	$t test -d $(APP)/deps/testsubdep
	$t printf "%s\n%s\n%s\n%s\n" \
		$(abspath $(APP)/deps/dep $(APP)/deps/subdep $(APP)/deps/testdep $(APP)/deps/testsubdep) \
		| sort > $(APP)/expected-test-deps.txt
	$t cmp $(APP)/expected-test-deps.txt $(APP)/.erlang.mk/recursive-test-deps-list.log
	$t $(MAKE) -C $(APP) --no-print-directory distclean $v

	$i "List application shell dependencies"
	$t $(MAKE) -C $(APP) --no-print-directory list-shell-deps $v
	$t test -d $(APP)/deps/subdep
	$t test -d $(APP)/deps/shellsubdep
	$t printf "%s\n%s\n%s\n%s\n" \
		$(abspath $(APP)/deps/dep $(APP)/deps/subdep $(APP)/deps/shelldep $(APP)/deps/shellsubdep) \
		| sort > $(APP)/expected-shell-deps.txt
	$t cmp $(APP)/expected-shell-deps.txt $(APP)/.erlang.mk/recursive-shell-deps-list.log
	$t $(MAKE) -C $(APP) --no-print-directory distclean $v

	$i "List application all dependencies (all kinds)"
	$t $(MAKE) -C $(APP) --no-print-directory list-deps DEP_TYPES='doc rel test shell' $v
	$t test -d $(APP)/deps/subdep
	$t test -d $(APP)/deps/docsubdep
	$t test -d $(APP)/deps/relsubdep
	$t test -d $(APP)/deps/testsubdep
	$t test -d $(APP)/deps/shellsubdep
	$t printf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n" \
		$(abspath \
			$(APP)/deps/dep $(APP)/deps/subdep \
			$(APP)/deps/docdep $(APP)/deps/docsubdep \
			$(APP)/deps/reldep $(APP)/deps/relsubdep \
			$(APP)/deps/testdep $(APP)/deps/testsubdep \
			$(APP)/deps/shelldep $(APP)/deps/shellsubdep) \
		| sort > $(APP)/expected-all-deps.txt
	$t cmp $(APP)/expected-all-deps.txt $(APP)/.erlang.mk/recursive-deps-list.log
	$t $(MAKE) -C $(APP) --no-print-directory distclean $v

core-deps-mv: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/cowlib

	$i "Move the application elsewhere"
	$t mv $(APP) $(APP)-moved

	$i "Build the application"
	$t $(MAKE) -C $(APP)-moved $v

core-deps-mv-rebar: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Lager to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/goldrush
	$t test -d $(APP)/deps/lager

	$i "Move the application elsewhere"
	$t mv $(APP) $(APP)-moved

	$i "Build the application"
	$t $(MAKE) -C $(APP)-moved $v

# A lower-level dependency of the first dependency always
# wins over a lower-level dependency of the second dependency.
core-deps-order-first: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy package and my_dep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep cowboy\ndep_my_dep = cp $(CURDIR)/$(APP)/my_dep/\n"}' $(APP)/Makefile

	$i "Bootstrap a new OTP library named my_dep inside $(APP)"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib 1.0.0 to the list of dependencies for my_dep"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = git https://github.com/ninenines/cowlib 1.0.0\n"}' $(APP)/my_dep/Makefile

ifdef LEGACY
	$i "Add Cowboy and my_dep to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n\t\tmy_dep,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/cowboy
	$t test -d $(APP)/deps/cowlib
	$t test -d $(APP)/deps/my_dep
	$t test -d $(APP)/deps/ranch

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), cowboy, cowlib, my_dep, ranch]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(cowboy, Deps), \
		{ok, \"1.0.0\"} = application:get_key(cowlib, vsn), \
		halt()"

# A higher-level dependency always wins.
core-deps-order-top: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy package and Cowlib 1.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy cowlib\ndep_cowlib = git https://github.com/ninenines/cowlib 1.0.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

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
		{ok, \"1.0.0\"} = application:get_key(cowlib, vsn), \
		halt()"

core-deps-no-autopatch: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy to the list of dependencies and Cowlib to the NO_AUTOPATCH list"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\nNO_AUTOPATCH = cowlib\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/cowboy
	$t test -d $(APP)/deps/cowlib
	$t test -d $(APP)/deps/ranch

	$i "Check that Cowlib was not autopatched"
	$t grep -q Hoguin $(APP)/deps/cowlib/erlang.mk

core-deps-no-autopatch-erlang-mk: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib to the list of dependencies and set NO_AUTOPATCH_ERLANG_MK=1"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\nNO_AUTOPATCH_ERLANG_MK = 1\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/cowlib

	$i "Check that Erlang.mk was not autopatched"
	$t grep -q Hoguin $(APP)/deps/cowlib/erlang.mk

core-deps-no-autopatch-rebar: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Lager to the list of dependencies and to the NO_AUTOPATCH list"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager\nNO_AUTOPATCH = lager\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/goldrush
	$t test -d $(APP)/deps/lager

	$i "Check that Lager was not autopatched"
	$t if grep -q erlang\.mk $(APP)/deps/goldrush/Makefile; then false; fi
	$t if grep -q erlang\.mk $(APP)/deps/lager/Makefile; then false; fi

ifndef LEGACY
core-deps-otp: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Crypto to the list of OTP dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = crypto\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that no dependencies were fetched"
	$t test ! -e $(APP)/deps

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), crypto]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(crypto, Deps), \
		halt()"
endif

core-deps-pkg: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

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

core-deps-rel: build clean

	$i "Bootstrap a new release-enabled OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib bootstrap-rel $v

	$i "Add Recon to the list of release dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "REL_DEPS = recon\n"}' $(APP)/Makefile

	$i "Add Recon to the relx.config file"
	$t $(ERL) -eval " \
		{ok, Conf0} = file:consult(\"$(APP)/relx.config\"), \
		Conf = lists:keyreplace(release, 1, Conf0, {release, {$(APP)_release, \"1\"}, [$(APP), recon]}), \
		ok = file:write_file(\"$(APP)/relx.config\", \
			lists:map(fun(Term) -> io_lib:format(\"~p.~n\", [Term]) end, Conf)), \
		halt()"

	$i "Build the application and its dependencies"
	$t $(MAKE) -C $(APP) deps app $v

	$i "Check that no dependencies were fetched"
	$t test ! -e $(APP)/deps

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(recon, Deps), \
		halt()"

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/recon

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), recon]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(recon, Deps), \
		halt()"

# @todo Add check for MSYS2 when releases under Windows become usable.
#	$i "Start the release and check that Recon is loaded"
ifeq ($(PLATFORM),msys2)
#	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd install $v
#	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd start $v
#	$t test -n "`$(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd rpcterms \
#		application loaded_applications | grep recon`"
#	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd stop $v
#	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd uninstall $v
else
	$i "Start the release and check that Recon is loaded"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release start $v
	$t test -n "`$(APP)/_rel/$(APP)_release/bin/$(APP)_release rpcterms \
		application loaded_applications | grep recon`"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release stop $v
endif

core-deps-search: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Run 'make search' and check that it prints packages"
	$t test -n "`$(MAKE) -C $(APP) search`"

	$i "Run 'make search q=cowboy' and check that it prints packages"
	$t test -n "`$(MAKE) -C $(APP) search q=cowboy`"

core-deps-shell: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add TDDReloader to the list of shell dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "SHELL_DEPS = tddreloader\n"}' $(APP)/Makefile

	$i "Build the application and its dependencies"
	$t $(MAKE) -C $(APP) deps app $v

	$i "Check that no dependencies were fetched"
	$t test ! -e $(APP)/deps

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(tddreloader, Deps), \
		halt()"

	$i "Run the shell"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="-eval \" \
		ok = application:load($(APP)), \
		ok = application:load(tddreloader), \
		halt()\"" $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/tddreloader

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), tddreloader]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(tddreloader, Deps), \
		halt()"

core-deps-skip: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application with SKIP_DEPS=1"
	$t $(MAKE) -C $(APP) SKIP_DEPS=1 $v

	$i "Check that no dependencies were fetched"
	$t test ! -e $(APP)/deps

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/cowlib
	$t test -d $(APP)/deps/cowboy
	$t test -d $(APP)/deps/ranch

	$i "Distclean with SKIP_DEPS=1"
	$t $(MAKE) -C $(APP) distclean SKIP_DEPS=1 $v

	$i "Check that no dependencies were removed"
	$t test -d $(APP)/deps/cowlib
	$t test -d $(APP)/deps/cowboy
	$t test -d $(APP)/deps/ranch

core-deps-test: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate .erl files"
	$t echo "-module(boy)." > $(APP)/src/boy.erl
	$t echo "-module(girl)." > $(APP)/src/girl.erl

	$i "Add triq to the list of test dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "TEST_DEPS = triq\n"}' $(APP)/Makefile

	$i "Build the application and its dependencies"
	$t $(MAKE) -C $(APP) deps app $v

	$i "Check that no dependencies were fetched"
	$t test ! -e $(APP)/deps

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(triq, Deps), \
		halt()"

	$i "Run tests"
	$t $(MAKE) -C $(APP) tests $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/triq

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), triq]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(triq, Deps), \
		halt()"
