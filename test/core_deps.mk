# Core: Packages and dependencies.

core_deps_TARGETS = $(call list_targets,core-deps)

.PHONY: core-deps $(core_deps_TARGETS)

core-deps: $(core_deps_TARGETS)

ifneq ($(PLATFORM),msys2)
core-deps-build-c-8cc: init

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

core-deps-build-c-lz4: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add lz4 to the list of build dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = lz4_src\ndep_lz4_src = git https://github.com/lz4/lz4 v1.8.2\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/lz4_src

	$i "Check that lz4 was compiled"
	$t test -f $(APP)/deps/lz4_src/lib/liblz4.a

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(lz4, Deps), \
		halt()"

core-deps-build-erl: init

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

core-deps-build-js: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add jquery to the list of build dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = jquery\ndep_jquery = git https://github.com/jquery/jquery main\n"}' $(APP)/Makefile

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

core-deps-cache-git: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/Makefile

	$i "Add CACHE_DEPS = 1 to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "CACHE_DEPS = 1\n"}' $(APP)/Makefile

	$i "Check that the cache doesn't exist yet"
	$t test ! -d $(CACHE_DIR)

	$i "Build the dependencies"
	$t $(MAKE) -C $(APP) deps $v

	$i "Check that the cache has been created"
	$t test -d $(CACHE_DIR)

	$i "Check that Cowlib was cloned in the cache"
	$t test -d $(CACHE_DIR)/git/cowlib

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that Cowlib is still in the cache"
	$t test -d $(CACHE_DIR)/git/cowlib

	$i "Break the Cowlib git link so we're forced to use the cache"
	$t echo 'dep_cowlib = git bad_url master' >> $(APP)/Makefile

	$i "Build the dependencies"
	$t $(MAKE) -C $(APP) deps $v

core-deps-cache-git-reuse: init

	$i "Bootstrap a new OTP library named $(APP)_1"
	$t mkdir $(APP)_1/
	$t cp ../erlang.mk $(APP)_1/
	$t $(MAKE) -C $(APP)_1 -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib 1.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = git \$$(pkg_cowlib_repo) 1.0.0\n"}' $(APP)_1/Makefile

	$i "Add CACHE_DEPS = 1 to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "CACHE_DEPS = 1\n"}' $(APP)_1/Makefile

	$i "Bootstrap a new OTP library named $(APP)_2"
	$t mkdir $(APP)_2/
	$t cp ../erlang.mk $(APP)_2/
	$t $(MAKE) -C $(APP)_2 -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib 2.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = git \$$(pkg_cowlib_repo) 2.0.0\n"}' $(APP)_2/Makefile

	$i "Add CACHE_DEPS = 1 to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "CACHE_DEPS = 1\n"}' $(APP)_2/Makefile

	$i "Build the dependencies in $(APP)_1"
	$t $(MAKE) -C $(APP)_1 deps $v

	$i "Check that the cache has been created"
	$t test -d $(CACHE_DIR)

	$i "Check that Cowlib was cloned in the cache"
	$t test -d $(CACHE_DIR)/git/cowlib

	$i "Build the dependencies in $(APP)_2"
	$t $(MAKE) -C $(APP)_2 deps $v

	$i "Check that $(APP)_1 cloned Cowlib 1.0.0"
	$t test "$$(cat $(APP)_1/deps/cowlib/.git/HEAD)" = "d544a494af4dbc810fc9c15eaf5cc050cced1501"

	$i "Check that $(APP)_2 cloned Cowlib 2.0.0"
	$t test "$$(cat $(APP)_2/deps/cowlib/.git/HEAD)" = "bd37be4d3b065600c3b76b492535e76e5d413fc1"

core-deps-cache-hex: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib to the list of dependencies using Hex"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = hex 2.12.1\n"}' $(APP)/Makefile

	$i "Add CACHE_DEPS = 1 to the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "CACHE_DEPS = 1\n"}' $(APP)/Makefile

	$i "Check that the cache doesn't exist yet"
	$t test ! -d $(CACHE_DIR)

	$i "Build the dependencies"
	$t $(MAKE) -C $(APP) deps $v

	$i "Check that the cache has been created"
	$t test -d $(CACHE_DIR)

	$i "Check that Cowlib was cloned in the cache"
	$t test -f $(CACHE_DIR)/hex/cowlib-2.12.1.tar

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that Cowlib is still in the cache"
	$t test -f $(CACHE_DIR)/hex/cowlib-2.12.1.tar

	$i "Build the dependencies"
	$t $(MAKE) -C $(APP) deps $v

core-deps-dep-built: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Touch one cowlib file to mark it for recompilation"
	$t $(SLEEP)
	$t touch $(APP)/deps/cowlib/src/cow_http.erl

	$i "Check that cowlib is not rebuilt"
	$t touch $(APP)/EXPECT
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) $v
	$t find $(APP)/deps/cowlib -type f -newer $(APP)/EXPECT | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT

	$i "Delete the dep_built file"
	$t rm $(APP)/deps/cowlib/ebin/dep_built

	$i "Check that cowlib was rebuilt"
	$t printf "%s\n" \
		$(APP)/deps/cowlib/cowlib.d \
		$(APP)/deps/cowlib/ebin/cowlib.app \
		$(APP)/deps/cowlib/ebin/cow_http.beam \
		$(APP)/deps/cowlib/ebin/dep_built | sort > $(APP)/EXPECT
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) $v
# Files in .git might end up modified due to the id generation in the .app file.
	$t find $(APP)/deps/cowlib -type f -newer $(APP)/EXPECT | grep -v ".git" | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT

core-deps-dep-built-full: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Touch one cowlib file to mark it for recompilation"
	$t $(SLEEP)
	$t touch $(APP)/deps/cowlib/src/cow_http.erl

	$i "Check that cowlib is rebuilt with FULL=1"
	$t printf "%s\n" \
		$(APP)/deps/cowlib/cowlib.d \
		$(APP)/deps/cowlib/ebin/cowlib.app \
		$(APP)/deps/cowlib/ebin/cow_http.beam \
		$(APP)/deps/cowlib/ebin/dep_built | sort > $(APP)/EXPECT
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) FULL=1 $v
# Files in .git might end up modified due to the id generation in the .app file.
	$t find $(APP)/deps/cowlib -type f -newer $(APP)/EXPECT | grep -v ".git" | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT

core-deps-dep-built-force-full: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Touch one cowlib file to mark it for recompilation"
	$t $(SLEEP)
	$t touch $(APP)/deps/cowlib/src/cow_http.erl

	$i "Check that cowlib is not rebuilt if \`force_rebuild_dep\` returns false"
	$t touch $(APP)/EXPECT
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) force_rebuild_dep='test $$(1) != $(CURDIR)/$(APP)/deps/cowlib' $v
	$t find $(APP)/deps/cowlib -type f -newer $(APP)/EXPECT | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT

	$i "Check that cowlib is rebuilt if \`force_rebuild_dep\` returns true"
	$t printf "%s\n" \
		$(APP)/deps/cowlib/cowlib.d \
		$(APP)/deps/cowlib/ebin/cowlib.app \
		$(APP)/deps/cowlib/ebin/cow_http.beam \
		$(APP)/deps/cowlib/ebin/dep_built | sort > $(APP)/EXPECT
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) force_rebuild_dep='test $$(1) = $(CURDIR)/$(APP)/deps/cowlib' $v
# Files in .git might end up modified due to the id generation in the .app file.
	$t find $(APP)/deps/cowlib -type f -newer $(APP)/EXPECT | grep -v ".git" | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT

	$i "Touch one cowlib file to mark it for recompilation"
	$t $(SLEEP)
	$t touch $(APP)/deps/cowlib/src/cow_http.erl

	$i "Check that cowlib is not rebuilt if \`FORCE_REBUILD\` is empty"
	$t touch $(APP)/EXPECT
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) FORCE_REBUILD= $v
	$t find $(APP)/deps/cowlib -type f -newer $(APP)/EXPECT | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT

	$i "Check that cowlib is not rebuilt if \`FORCE_REBUILD\` does not mention cowlib"
	$t touch $(APP)/EXPECT
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) FORCE_REBUILD='other_dep' $v
	$t find $(APP)/deps/cowlib -type f -newer $(APP)/EXPECT | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT

	$i "Check that cowlib is rebuilt if \`FORCE_REBUILD\` contains cowlib"
	$t printf "%s\n" \
		$(APP)/deps/cowlib/cowlib.d \
		$(APP)/deps/cowlib/ebin/cowlib.app \
		$(APP)/deps/cowlib/ebin/cow_http.beam \
		$(APP)/deps/cowlib/ebin/dep_built | sort > $(APP)/EXPECT
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) FORCE_REBUILD='other_dep cowlib' $v
# Files in .git might end up modified due to the id generation in the .app file.
	$t find $(APP)/deps/cowlib -type f -newer $(APP)/EXPECT | grep -v ".git" | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT

core-deps-dep-built-ln: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/Makefile

	$i "Clone cowlib manually inside $(APP)"
	$t git clone -q https://github.com/ninenines/cowlib $(APP)/cowlib

	$i "Link to cowlib instead of fetching the dependency"
	$t mkdir -p $(APP)/deps
	$t ln -s ../cowlib $(APP)/deps/cowlib

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

# On MSYS2 "ln" will by default not create symbolic links because
# it requires an option to be enabled and administrative privileges.
# The "rebuild" part of the test is therefore skipped on Windows.
ifneq ($(PLATFORM),msys2)
	$i "Touch one cowlib file to mark it for recompilation"
	$t $(SLEEP)
	$t touch $(APP)/deps/cowlib/src/cow_http.erl

	$i "Check that cowlib is rebuilt; symlinked deps don't create dep_built"
	$t printf "%s\n" \
		$(APP)/cowlib/cowlib.d \
		$(APP)/cowlib/ebin/cowlib.app \
		$(APP)/cowlib/ebin/cow_http.beam | sort > $(APP)/EXPECT

	$t $(SLEEP)
	$t $(MAKE) -C $(APP) $v
# Files in .git might end up modified due to the id generation in the .app file.
	$t find $(APP)/cowlib -type f -newer $(APP)/EXPECT | grep -v ".git" | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT
endif

core-deps-dep-commit: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.12.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy_commit = 2.12.0\n"}' $(APP)/Makefile

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
		{ok, \"2.12.0\"} = application:get_key(cowboy, vsn), \
		halt()"

core-deps-dir: init

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

core-deps-doc: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate .erl files"
	$t echo "-module(boy)." > $(APP)/src/boy.erl
	$t echo "-module(girl)." > $(APP)/src/girl.erl

	$i "Add Edown as a documentation building dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DOC_DEPS = edown\ndep_edown = git https://github.com/uwiger/edown master\nEDOC_OPTS = {doclet, edown_doclet}\n"}' $(APP)/Makefile

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

core-deps-fetch-cp: init

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

core-deps-fetch-custom: init

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

core-deps-fetch-fail-bad: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib as a dependency using a non-existing fetch method named oops"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = oops https://github.com/ninenines/cowlib 1.0.0\n"}' $(APP)/Makefile

	$i "Check that building the application fails"
	$t ! $(MAKE) -C $(APP) $v

core-deps-fetch-fail-unknown: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add an unknown application as a dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = unknown\n"}' $(APP)/Makefile

	$i "Check that building the application fails"
	$t ! $(MAKE) -C $(APP) $v

core-deps-fetch-git: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.12.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy_commit = 2.12.0\n"}' $(APP)/Makefile

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
		{ok, \"2.12.0\"} = application:get_key(cowboy, vsn), \
		halt()"

core-deps-fetch-git-subfolder: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Bootstrap a new OTP library named my_dep as a subfolder inside $(APP)"
	$t mkdir -p $(APP)/git_repo/my_dep
	$t cp ../erlang.mk $(APP)/git_repo/my_dep/
	$t $(MAKE) -C $(APP)/git_repo/my_dep/ -f erlang.mk bootstrap-lib $v
# Create an empty file so src/ gets committed.
	$t touch $(APP)/git_repo/my_dep/src/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add my_dep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git-subfolder file://$(abspath $(APP)/git_repo) master my_dep\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add my_dep to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tmy_dep,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the dependency was fetched"
	$t test -d $(APP)/deps/my_dep

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), my_dep]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(my_dep, Deps), \
		halt()"

core-deps-fetch-git-submodule: init

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
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add the submodule to my_dep"
	$t mkdir $(APP)/deps
	$t cd $(APP) && \
		git init -q -b master && \
		git -c protocol.file.allow=always submodule -q add file://$(abspath $(APP)/my_dep) deps/my_dep && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

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

core-deps-fetch-hex: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.12.0 and SystemD 0.6.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy systemd\ndep_cowboy = hex 2.12.0\ndep_systemd = hex 0.6.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy and SystemD to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n\t\tsystemd,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/cowboy
	$t test -d $(APP)/deps/cowlib
	$t test -d $(APP)/deps/ranch
	$t test -d $(APP)/deps/systemd
	$t test -d $(APP)/deps/enough

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), cowboy, cowlib, ranch, systemd, enough]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(cowboy, Deps), \
		true = lists:member(systemd, Deps), \
		{ok, \"2.12.0\"} = application:get_key(cowboy, vsn), \
		{ok, \"0.6.0\"} = application:get_key(systemd, vsn), \
		halt()"

# @todo Enable this test again when a host provides Mercurial again.
#core-deps-fetch-hg: init
#
#	$i "Bootstrap a new OTP library named $(APP)"
#	$t mkdir $(APP)/
#	$t cp ../erlang.mk $(APP)/
#	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v
#
#	$i "Add Ehsa 4.0.3 to the list of dependencies"
#	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = ehsa\ndep_ehsa = hg https://bitbucket.org/a12n/ehsa 4.0.3\n"}' $(APP)/Makefile
#
#ifdef LEGACY
#	$i "Add ehsa to the applications key in the .app.src file"
#	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tehsa,\n"}' $(APP)/src/$(APP).app.src
#endif
#
#	$i "Build the application"
#	$t $(MAKE) -C $(APP) $v
#
#	$i "Check that all dependencies were fetched"
#	$t test -d $(APP)/deps/ehsa
#
#	$i "Check that the application was compiled correctly"
#	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
#		[ok = application:load(App) || App <- [$(APP), ehsa]], \
#		{ok, Deps} = application:get_key($(APP), applications), \
#		true = lists:member(ehsa, Deps), \
#		{ok, \"4.0.3\"} = application:get_key(ehsa, vsn), \
#		halt()"

core-deps-fetch-ln: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Bootstrap a new OTP library named my_dep inside $(APP)"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap-lib $v

	$i "Add my_dep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = ln $(CURDIR)/$(APP)/my_dep/\n"}' $(APP)/Makefile

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

# @todo Enable this test again when a host provides Subversion again.
#core-deps-fetch-svn: init
#
#	$i "Bootstrap a new OTP library named $(APP)"
#	$t mkdir $(APP)/
#	$t cp ../erlang.mk $(APP)/
#	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v
#
#	$i "Add Cowlib 1.0.0 to the list of dependencies"
#	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = svn https://github.com/ninenines/cowlib/tags/1.0.0\n"}' $(APP)/Makefile
#
#ifdef LEGACY
#	$i "Add Cowlib to the applications key in the .app.src file"
#	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowlib,\n"}' $(APP)/src/$(APP).app.src
#endif
#
#	$i "Build the application"
#	$t $(MAKE) -C $(APP) $v
#
#	$i "Check that all dependencies were fetched"
#	$t test -d $(APP)/deps/cowlib
#
#	$i "Check that the application was compiled correctly"
#	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
#		[ok = application:load(App) || App <- [$(APP), cowlib]], \
#		{ok, Deps} = application:get_key($(APP), applications), \
#		true = lists:member(cowlib, Deps), \
#		{ok, \"1.0.0\"} = application:get_key(cowlib, vsn), \
#		halt()"

core-deps-ignore: init

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
	$i "Bootstrap a new OTP library named $(APP)_$(1)subdep"
	$t mkdir $(APP)_$(1)subdep/
	$t cp ../erlang.mk $(APP)_$(1)subdep/
	$t $2 -C $(APP)_$(1)subdep --no-print-directory -f erlang.mk bootstrap-lib $$v

	$i "Create a Git repository for $(APP)_$(1)subdep"
	$t (cd $(APP)_$(1)subdep && \
		git init -q -b master && \
		git config user.name "Testsuite" && \
		git config user.email "testsuite@erlang.mk" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Initial commit")

	$i "Bootstrap a new OTP library named $(APP)_$(1)dep"
	$t mkdir $(APP)_$(1)dep/
	$t cp ../erlang.mk $(APP)_$(1)dep/
	$t $2 -C $(APP)_$(1)dep --no-print-directory -f erlang.mk bootstrap-lib $$v

	$i "Add $(APP)_$(1)subdep as a dependency"
	$t perl -ni.bak -e \
		'print;if ($$.==1) {print "DEPS = $(1)subdep\ndep_$(1)subdep = git file://$(abspath $(APP)_$(1)subdep) master\n"}' \
		$(APP)_$(1)dep/Makefile
	$t rm $(APP)_$(1)dep/Makefile.bak

	$i "Create a Git repository for $(APP)_$(1)dep"
	$t (cd $(APP)_$(1)dep && \
		git init -q -b master && \
		git config user.name "Testsuite" && \
		git config user.email "testsuite@erlang.mk" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Initial commit")
endef

core-deps-list-deps: init

# We pass $(MAKE) directly so that GNU Make can pass its context forward.
# If we didn't then $(MAKE) would be expanded in the call without context.
	$(call add_dep_and_subdep,,$(MAKE))
	$(call add_dep_and_subdep,doc,$(MAKE))
	$(call add_dep_and_subdep,rel,$(MAKE))
	$(call add_dep_and_subdep,test,$(MAKE))
	$(call add_dep_and_subdep,shell,$(MAKE))

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
dep_dep = git file://$(abspath $(APP)_dep) master\
dep_docdep = git file://$(abspath $(APP)_docdep) master\
dep_reldep = git file://$(abspath $(APP)_reldep) master\
dep_testdep = git file://$(abspath $(APP)_testdep) master\
dep_shelldep = git file://$(abspath $(APP)_shelldep) master\
' $(APP)/Makefile
	$t rm $(APP)/Makefile.bak

	$i "Create a Git repository for $(APP)"
	$t (cd $(APP) && \
		git init -q -b master && \
		git config user.name "Testsuite" && \
		git config user.email "testsuite@erlang.mk" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Initial commit")

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

core-deps-list-deps-with-apps: init

# We pass $(MAKE) directly so that GNU Make can pass its context forward.
# If we didn't then $(MAKE) would be expanded in the call without context.
	$(call add_dep_and_subdep,,$(MAKE))

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Bootstrap another APP named $(APP)_app in $(APP) repository"
	$t $(MAKE) -C $(APP) --no-print-directory new-lib in=my_app $v

	$i "Bootstrap another APP named $(APP)_dep_app in $(APP)_dep repository"
	$t $(MAKE) -C $(APP)_dep --no-print-directory new-lib in=my_dep_app $v
	$t sed -i.bak '2i\
APPS_DIR := $$(CURDIR)/apps\
LOCAL_DEPS = my_dep_app ssl\
' $(APP)_dep/Makefile
	$t echo 'unexport APPS_DIR' >> $(APP)_dep/Makefile
	$t rm $(APP)_dep/Makefile.bak
	$t cd $(APP)_dep && git add .
	$t cd $(APP)_dep && git commit -q --no-gpg-sign -m "Add application"

	$i "Add $(APP)-dep as a dependency"
	$t sed -i.bak '2i\
DEPS = dep\
dep_dep = git file://$(abspath $(APP)_dep) master\
' $(APP)/Makefile
	$t rm $(APP)/Makefile.bak

	$i "Create a Git repository for $(APP)"
	$t (cd $(APP) && \
		git init -q -b master && \
		git config user.name "Testsuite" && \
		git config user.email "testsuite@erlang.mk" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Initial commit")

	$i "List application dependencies ($(APP)_app is missing)"
	$t $(MAKE) -C $(APP) --no-print-directory list-deps $v
	$t test -d $(APP)/deps/subdep
	$t printf "%s\n%s\n%s\n" $(abspath $(APP)/deps/dep $(APP)/deps/dep/apps/my_dep_app $(APP)/deps/subdep) > $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/recursive-deps-list.log
	$t $(MAKE) -C $(APP) --no-print-directory distclean $v

	$i "Add $(APP)_app as a dependency"
	$t sed -i.bak '3i\
LOCAL_DEPS = my_app\
' $(APP)/Makefile
	$t rm $(APP)/Makefile.bak

	$i "List application dependencies ($(APP)_app is listed)"
	$t $(MAKE) -C $(APP) --no-print-directory list-deps $v
	$t test -d $(APP)/deps/subdep
	$t printf "%s\n%s\n%s\n%s\n" $(abspath $(APP)/apps/my_app $(APP)/deps/dep $(APP)/deps/dep/apps/my_dep_app $(APP)/deps/subdep) > $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/recursive-deps-list.log
	$t $(MAKE) -C $(APP) --no-print-directory distclean $v

core-deps-list-deps-with-specified-full-var: init

# We pass $(MAKE) directly so that GNU Make can pass its context forward.
# If we didn't then $(MAKE) would be expanded in the call without context.
	$(call add_dep_and_subdep,,$(MAKE))

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add $(APP)-dep as a dependency"
	$t sed -i.bak '2i\
DEPS = dep\
dep_dep = git file://$(abspath $(APP)_dep) master\
' $(APP)/Makefile
	$t rm $(APP)/Makefile.bak

	$i "Create a Git repository for $(APP)"
	$t (cd $(APP) && \
		git init -q -b master && \
		git config user.name "Testsuite" && \
		git config user.email "testsuite@erlang.mk" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Initial commit")

	$i "Build application"
	$t $(MAKE) -C $(APP) --no-print-directory $v
	$t test -d $(APP)/deps/subdep

	$i "List application dependencies without FULL"
	$t $(MAKE) -C $(APP) --no-print-directory list-deps $v
	$t printf "%s\n%s\n" $(abspath $(APP)/deps/dep $(APP)/deps/subdep) > $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/recursive-deps-list.log

	$i "List application dependencies with empty FULL"
	$t $(MAKE) -C $(APP) --no-print-directory list-deps FULL= $v
	$t printf "%s\n%s\n" $(abspath $(APP)/deps/dep $(APP)/deps/subdep) > $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/recursive-deps-list.log

	$i "List application dependencies with FULL=1"
	$t $(MAKE) -C $(APP) --no-print-directory list-deps FULL=1 $v
	$t printf "%s\n%s\n" $(abspath $(APP)/deps/dep $(APP)/deps/subdep) > $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/recursive-deps-list.log

	$t $(MAKE) -C $(APP) --no-print-directory distclean $v

core-deps-makefile-change: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Add Cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowlib to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowlib,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application again"
	$t $(MAKE) -C $(APP) $v

	$i "Check that Cowlib was included in the .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, Apps} = application:get_key($(APP), applications), \
		true = lists:member(cowlib, Apps), \
		halt()."

core-deps-dep-makefile-change: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Bootstrap a new OTP library named my_dep inside $(APP)"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap-lib $v

	$i "Add my_dep to the list of dependencies"
# We use FORCE_REBUILD to ensure it gets rebuilt even on Windows.
	$t perl -ni.bak -e "print;if ($$.==1) {print \"DEPS = my_dep\ndep_my_dep = ln $(CURDIR)/$(APP)/my_dep/\nFORCE_REBUILD = my_dep\n\"}" $(APP)/Makefile

ifdef LEGACY
	$i "Add my_dep to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tmy_dep,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) NO_AUTOPATCH=my_dep $v

	$i "Add Cowlib to the list of dependencies in my_dep"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib_commit = master\n"}' $(APP)/deps/my_dep/Makefile

ifdef LEGACY
	$i "Add Cowlib to the applications key in my_dep's .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowlib,\n"}' $(APP)/deps/my_dep/src/my_dep.app.src
endif

	$i "Build the application again"
	$t $(MAKE) -C $(APP) $v

	$i "Check that Cowlib was included in my_dep's .app file"
	$t $(ERL) -pa $(APP)/deps/my_dep/ebin/ -eval " \
		ok = application:load(my_dep), \
		{ok, Apps} = application:get_key(my_dep, applications), \
		true = lists:member(cowlib, Apps), \
		halt()."

core-deps-mv: init

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

core-deps-mv-rebar: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Lager to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager\ndep_lager = git https://github.com/erlang-lager/lager master\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/goldrush
	$t test -d $(APP)/deps/lager

	$i "Move the application elsewhere"
	$t mv $(APP) $(APP)-moved

	$i "Build the application"
	$t $(MAKE) -C $(APP)-moved $v

ifndef LEGACY
core-deps-optional: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add quicer to the list of optional dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "OPTIONAL_DEPS = quicer\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that no dependencies were fetched"
	$t test ! -e $(APP)/deps

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(quicer, Deps), \
		{ok, [quicer]} = application:get_key($(APP), optional_applications), \
		halt()"
endif

# A lower-level dependency of the first dependency always
# wins over a lower-level dependency of the second dependency.
core-deps-order-first: init

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

	$i "Add Cowlib 2.0.0 to the list of dependencies for my_dep"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = git https://github.com/ninenines/cowlib 2.0.0\n"}' $(APP)/my_dep/Makefile

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
		{ok, \"2.0.0\"} = application:get_key(cowlib, vsn), \
		halt()"

# A higher-level dependency always wins.
core-deps-order-top: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy package and Cowlib 2.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy cowlib\ndep_cowlib = git https://github.com/ninenines/cowlib 2.0.0\n"}' $(APP)/Makefile

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
		{ok, \"2.0.0\"} = application:get_key(cowlib, vsn), \
		halt()"

ifndef LEGACY
core-deps-otp: init

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

core-deps-pkg: init

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

core-deps-rel: init

	$i "Bootstrap a new release-enabled OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib bootstrap-rel $v

	$i "Add Recon to the list of release dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "REL_DEPS = recon\ndep_recon = git https://github.com/ferd/recon master\n"}' $(APP)/Makefile

	$i "Add Recon to the relx.config file"
	$t $(ERL) -eval " \
		{ok, Conf0} = file:consult(\"$(APP)/relx.config\"), \
		Conf = lists:keyreplace(release, 1, Conf0, {release, {$(APP)_release, \"1\"}, [$(APP), recon]}), \
		ok = file:write_file(\"$(APP)/relx.config\", \
			lists:map(fun(Term) -> io_lib:format(\"~p.~n\", [Term]) end, Conf)), \
		halt()"

	$i "Build the application and its dependencies"
	$t $(MAKE) -C $(APP) deps app $v

	$i "Check that Recon was not fetched"
	$t test ! -e $(APP)/deps/recon

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(recon, Deps), \
		halt()"

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that Recon was fetched"
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
#	$t test -n "`$(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd rpc \
#		application loaded_applications | grep recon`"
#	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd stop $v
#	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd uninstall $v
else
	$i "Start the release and check that Recon is loaded"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release daemon $v
	$t apps="Node is not running!"; \
		while test "$$apps" = "Node is not running!"; do \
			apps=$$($(APP)/_rel/$(APP)_release/bin/$(APP)_release \
			rpc application loaded_applications); \
		done; \
		echo "$$apps" | grep -q recon
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release stop $v
endif

core-deps-search: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Run 'make search' and check that it prints packages"
	$t test -n "`$(MAKE) -C $(APP) search`"

	$i "Run 'make search q=cowboy' and check that it prints packages"
	$t test -n "`$(MAKE) -C $(APP) search q=cowboy`"

core-deps-shell: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Bootstrap a new OTP application named my_dep inside $(APP)"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap $v

	$i "Add my_dep to the list of dependencies"
# We use FORCE_REBUILD to ensure it gets rebuilt even on Windows.
	$t perl -ni.bak -e "print;if ($$.==1) {print \"SHELL_DEPS = my_dep\ndep_my_dep = ln $(CURDIR)/$(APP)/my_dep/\nFORCE_REBUILD = my_dep\n\"}" $(APP)/Makefile

	$i "Build the application and its dependencies"
	$t $(MAKE) -C $(APP) deps app $v

	$i "Check that no dependencies were fetched"
	$t test ! -e $(APP)/deps

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP)]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(my_dep, Deps), \
		halt()"

	$i "Run the shell"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="-eval \" \
		ok = application:load($(APP)), \
		ok = application:load(my_dep), \
		halt()\"" $v

	$i "Check that all dependencies were fetched"
	$t test -d $(APP)/deps/my_dep

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/deps/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), my_dep]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		false = lists:member(my_dep, Deps), \
		halt()"

core-deps-skip: init

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

core-deps-test: init

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
