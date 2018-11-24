# Core: Multi-applications.

CORE_APPS_TARGETS = $(call list_targets,core-apps)

.PHONY: core-apps $(CORE_APPS_TARGETS)

core-apps: $(CORE_APPS_TARGETS)

core-apps-build: build clean

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
	$t test -f $(APP)/deps/cowlib/ebin/cowlib.app

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

	$i "Clean Cowlib"
	$t $(MAKE) -C $(APP)/deps/cowlib clean $v

	$i "Check that Cowlib compiled files were removed"
	$t test ! -e $(APP)/deps/cowlib/ebin/cowlib.app

	$i "Build the application again"
	$t $(MAKE) -C $(APP) $v

	$i "Check that Cowlib compiled files exist"
	$t test -f $(APP)/deps/cowlib/ebin/cowlib.app

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

core-apps-build-count: build clean

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
	$t echo "	@printf '#' >> count" >> $(APP)/apps/app_one/Makefile

	$i "Create a new application app_two"
	$t $(MAKE) -C $(APP) new-app in=app_two $v
	$t echo "all::" >> $(APP)/apps/app_two/Makefile
	$t echo "	@printf '#' >> count" >> $(APP)/apps/app_two/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check the number of times each app was compiled"
	$t test "`wc -c $(APP)/apps/app_one/count | awk '{printf $$1}'`" -eq 1
	$t test "`wc -c $(APP)/apps/app_two/count | awk '{printf $$1}'`" -eq 1

core-apps-conflict: build clean

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

core-apps-deep-conflict: build clean

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

core-apps-dir: build clean

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

core-apps-dir-include-lib: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Set a custom APPS_DIR"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "APPS_DIR ?= \$$(CURDIR)/deep/libs\n"}' $(APP)/Makefile

	$i "Create new libraries boy_app and girl_app"
	$t $(MAKE) -C $(APP) new-lib in=boy_app $v
	$t $(MAKE) -C $(APP) new-lib in=girl_app $v

	$i "Make the applications depend on each other"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = boy_app\n"}' $(APP)/deep/libs/girl_app/Makefile
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = girl_app\n"}' $(APP)/deep/libs/boy_app/Makefile

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

	$i "Build in a subdirectory"
	$t $(MAKE) -C $(APP)/deep/libs/boy_app $v

	$i "Check that all compiled files exist (excluding the top-level app)"
	$t ! test -f $(APP)/$(APP).d
	$t ! test -f $(APP)/ebin/$(APP).app

	$t test -f $(APP)/deep/libs/boy_app/boy_app.d
	$t test -f $(APP)/deep/libs/boy_app/ebin/boy_app.app
	$t test -f $(APP)/deep/libs/boy_app/ebin/boy.beam

	$t test -f $(APP)/deep/libs/girl_app/girl_app.d
	$t test -f $(APP)/deep/libs/girl_app/ebin/girl_app.app
	$t test -f $(APP)/deep/libs/girl_app/ebin/girl.beam

core-apps-new-app: build clean

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

core-apps-new-lib: build clean

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

core-apps-new-tpl: build clean

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

core-apps-local-deps: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a new application my_app_1"
	$t $(MAKE) -C $(APP) new-app in=my_app_1 $v

	$i "Add Lager to the list of dependencies of my_app_1, and add the lager parse_transform"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager\nERLC_OPTS+=+{parse_transform,lager_transform}\n"}' $(APP)/apps/my_app_1/Makefile

	$i "Add a lager:error/2 call to my_app_1_app.erl that will fail if the parse_transform doesn't run"
	$t perl -ni.bak -e 'print;if (/^-export/){print "\n-export([log/0]).\n"} if (eof) {print "\nlog() -> lager:error(\"test\", []).\n"}' $(APP)/apps/my_app_1/src/my_app_1_app.erl

	$i "Create a new application my_app_2"
	$t $(MAKE) -C $(APP) new-app in=my_app_2 $v

	$i "Add my_app_1 to the list of local dependencies of my_app_2, don't add lager, but add the lager parse_transform (this will fail unless my_app_1 was indeed built first)"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = my_app_1\nERLC_OPTS+=+{parse_transform,lager_transform}\n"}' $(APP)/apps/my_app_2/Makefile

	$i "Add a lager:error/2 call to my_app_2_app.erl that will fail if the parse_transform doesn't run"
	$t perl -ni.bak -e 'print;if (/^-export/){print "\n-export([log/0]).\n"} if (eof) {print "\nlog() -> lager:error(\"test\", []).\n"}' $(APP)/apps/my_app_2/src/my_app_2_app.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/apps/my_app_1/my_app_1.d
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1.app
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1_app.beam
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1_sup.beam
	$t test -f $(APP)/apps/my_app_2/my_app_2.d
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2.app
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2_app.beam
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2_sup.beam
	$t test -f $(APP)/deps/lager/ebin/lager.app

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Test after swapping my_app_1 and my_app_2 to make sure lexical ordering didnt incidentally build the correct app first"

	$i "Add my_app_2 to the list of local dependencies of my_app_1, don't add lager, but add the lager parse_transform (this will fail unless my_app_2 was indeed built first)"
	$t mv $(APP)/apps/my_app_1/Makefile{.bak,}
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = my_app_2\nERLC_OPTS+=+{parse_transform,lager_transform}\n"}' $(APP)/apps/my_app_1/Makefile

	$i "Add Lager to the list of dependencies of my_app_2, and add the lager parse_transform"
	$t mv $(APP)/apps/my_app_2/Makefile{.bak,}
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager\nERLC_OPTS+=+{parse_transform,lager_transform}\n"}' $(APP)/apps/my_app_2/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/apps/my_app_1/my_app_1.d
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1.app
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1_app.beam
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1_sup.beam
	$t test -f $(APP)/apps/my_app_2/my_app_2.d
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2.app
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2_app.beam
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2_sup.beam
	$t test -f $(APP)/deps/lager/ebin/lager.app

core-apps-local-deps-circular: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a new application my_app_1"
	$t $(MAKE) -C $(APP) new-app in=my_app_1 $v

	$i "Create a new application my_app_2"
	$t $(MAKE) -C $(APP) new-app in=my_app_2 $v

	$i "Add my_app_1 to the list of local dependencies of my_app_2"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = my_app_1\n"}' $(APP)/apps/my_app_2/Makefile

	$i "Add my_app_2 to the list of local dependencies of my_app_1"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = my_app_2\n"}' $(APP)/apps/my_app_1/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/apps/my_app_1/my_app_1.d
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1.app
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1_app.beam
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1_sup.beam
	$t test -f $(APP)/apps/my_app_2/my_app_2.d
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2.app
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2_app.beam
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2_sup.beam

core-apps-only: build clean

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
	$t test -f $(APP)/deps/cowlib/ebin/cowlib.app

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/apps/*/ebin/ -eval " \
		ok = application:load(my_app), \
		{ok, Mods = [my_app_app, my_app_sup, my_server]} = application:get_key(my_app, modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"

	$i "Clean Cowlib"
	$t $(MAKE) -C $(APP)/deps/cowlib clean $v

	$i "Check that Cowlib compiled files were removed"
	$t test ! -e $(APP)/deps/cowlib/ebin/cowlib.app

	$i "Build the application again"
	$t $(MAKE) -C $(APP) $v

	$i "Check that Cowlib compiled files exist"
	$t test -f $(APP)/deps/cowlib/ebin/cowlib.app

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

core-apps-toplevel-local-deps: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a new application my_app_1"
	$t $(MAKE) -C $(APP) new-app in=my_app_1 $v

	$i "Add my_app_1 to the list of toplevel local dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = my_app_1\n"}' $(APP)/Makefile

	$i "Create a new application my_app_2"
	$t $(MAKE) -C $(APP) new-app in=my_app_2 $v

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/$(APP).d
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/apps/my_app_1/my_app_1.d
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1.app
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1_app.beam
	$t test -f $(APP)/apps/my_app_1/ebin/my_app_1_sup.beam

	$i "Check that my_app_2 compiled files DO NOT exist"
	$t test ! -e $(APP)/apps/my_app_2/my_app_2.d
	$t test ! -e $(APP)/apps/my_app_2/ebin/my_app_2.app
	$t test ! -e $(APP)/apps/my_app_2/ebin/my_app_2_app.beam
	$t test ! -e $(APP)/apps/my_app_2/ebin/my_app_2_sup.beam

	$i "Add my_app_2 to the list of local dependencies of my_app_1"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = my_app_2\n"}' $(APP)/apps/my_app_1/Makefile

	$i "Rebuild the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that my_app_2 compiled files exist"
	$t test -f $(APP)/apps/my_app_2/my_app_2.d
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2.app
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2_app.beam
	$t test -f $(APP)/apps/my_app_2/ebin/my_app_2_sup.beam
