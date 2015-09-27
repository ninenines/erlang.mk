# Core: Packages and dependencies.

CORE_DEPS_CASES = build-c-8cc build-c-imagejs build-erl build-js dep-commit dir doc fetch-cp fetch-custom fetch-fail-bad fetch-fail-unknown fetch-git fetch-hex fetch-hg fetch-legacy fetch-svn ignore order-first order-top otp pkg rel search shell test
CORE_DEPS_TARGETS = $(addprefix core-deps-,$(CORE_DEPS_CASES))
CORE_DEPS_CLEAN_TARGETS = $(addprefix clean-,$(CORE_DEPS_TARGETS))

.PHONY: core-deps $(CORE_DEPS_TARGETS) clean-core-deps $(CORE_DEPS_CLEAN_TARGETS)

clean-core-deps: $(CORE_DEPS_CLEAN_TARGETS)

$(CORE_DEPS_CLEAN_TARGETS):
	$t rm -rf $(APP_TO_CLEAN)/

core-deps: $(CORE_DEPS_TARGETS)

ifneq ($(PLATFORM),msys2)
core-deps-build-c-8cc: build clean-core-deps-build-c-8cc

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

core-deps-build-c-imagejs: build clean-core-deps-build-c-imagejs

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

core-deps-build-erl: build clean-core-deps-build-erl

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

core-deps-build-js: build clean-core-deps-build-js

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

core-deps-dep-commit: build clean-core-deps-dep-commit

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

core-deps-dir: build clean-core-deps-dir

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy to the list of dependencies with a custom DEPS_DIR"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\nDEPS_DIR ?= \$$(CURDIR)/libs\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched in the custom DEPS_DIR"
	$t test -d $(APP)/libs/cowboy
	$t test -d $(APP)/libs/cowlib
	$t test -d $(APP)/libs/ranch

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/libs/*/ebin/ -eval " \
		[ok = application:load(App) || App <- [$(APP), cowboy, cowlib, ranch]], \
		{ok, Deps} = application:get_key($(APP), applications), \
		true = lists:member(cowboy, Deps), \
		halt()"

core-deps-doc: build clean-core-deps-doc

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
	$t $(MAKE) -C $(APP) docs $v

	$i "Check that documentation dependencies were fetched"
	$t test -d $(APP)/deps/edown

	$i "Check the Edown generated Markdown documentation"
	$t test -f $(APP)/doc/boy.md
	$t test -f $(APP)/doc/girl.md

core-deps-fetch-cp: build clean-core-deps-fetch-cp

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

core-deps-fetch-custom: build clean-core-deps-fetch-custom

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

core-deps-fetch-fail-bad: build clean-core-deps-fetch-fail-bad

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib as a dependency using a non-existing fetch method named oops"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = oops https://github.com/ninenines/cowlib 1.0.0\n"}' $(APP)/Makefile

	$i "Check that building the application fails"
	$t if $(MAKE) -C $(APP) $v; then false; fi

core-deps-fetch-fail-unknown: build clean-core-deps-fetch-fail-unknown

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add an unknown application as a dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = unknown\n"}' $(APP)/Makefile

	$i "Check that building the application fails"
	$t if $(MAKE) -C $(APP) $v; then false; fi

core-deps-fetch-git: build clean-core-deps-fetch-git

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

core-deps-fetch-hex: build clean-core-deps-fetch-hex

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

core-deps-fetch-hg: build clean-core-deps-fetch-hg

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
core-deps-fetch-legacy: build clean-core-deps-fetch-legacy

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib as a dependency using a non-existing fetch method named oops"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = https://github.com/ninenines/cowlib 1.0.0\n"}' $(APP)/Makefile

	$i "Check that building the application fails"
	$t if $(MAKE) -C $(APP) $v; then false; fi

	$i "Check that building the application works with IS_DEP=1"
	$t $(MAKE) -C $(APP) IS_DEP=1 $v

core-deps-fetch-svn: build clean-core-deps-fetch-svn

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

core-deps-ignore: build clean-core-deps-ignore

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

# A lower-level dependency of the first dependency always
# wins over a lower-level dependency of the second dependency.
core-deps-order-first: build clean-core-deps-order-first

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
core-deps-order-top: build clean-core-deps-order-top

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

ifndef LEGACY
core-deps-otp: build clean-core-deps-otp

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Crypto to the list of OTP dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "OTP_DEPS = crypto\n"}' $(APP)/Makefile

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

core-deps-pkg: build clean-core-deps-pkg

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

core-deps-rel: build clean-core-deps-rel

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

core-deps-search: build clean-core-deps-search

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Run 'make search' and check that it prints packages"
	$t test -n "`$(MAKE) -C $(APP) search`"

	$i "Run 'make search q=cowboy' and check that it prints packages"
	$t test -n "`$(MAKE) -C $(APP) search q=cowboy`"

core-deps-shell: build clean-core-deps-shell

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

core-deps-test: build clean-core-deps-test

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
