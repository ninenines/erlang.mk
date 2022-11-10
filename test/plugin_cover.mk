# Common Test plugin.

COVER_TARGETS = $(call list_targets,cover)

.PHONY: cover $(COVER_TARGETS)

cover: $(COVER_TARGETS)

cover-ct: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> application:start($(APP))." > $(APP)/test/$(APP)_SUITE.erl

	$i "Run Common Test with code coverage enabled"
	$t $(MAKE) -C $(APP) ct COVER=1 $v

	$i "Check that the generated files exist"
	$t test -f $(APP)/cover/ct.coverdata
	$t test -f $(APP)/test/ct.cover.spec

	$i "Check that the generated files are removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/cover/ct.coverdata
	$t test ! -e $(APP)/test/ct.cover.spec

cover-ct-incl-apps: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a new application app_one"
	$t $(MAKE) -C $(APP) new-app in=app_one $v

	$i "Create a new application app_two"
	$t $(MAKE) -C $(APP) new-app in=app_two $v

	$i "Create a new application app_three"
	$t $(MAKE) -C $(APP) new-app in=app_three $v

	$i "Add all three apps to LOCAL_DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = app_one app_two app_three\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add all three apps to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tapp_one,\n\t\tapp_two,\n\t\tapp_three,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Add app_one and app_three to the code coverage"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "COVER_APPS = app_one app_three\n"}' $(APP)/Makefile

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> application:ensure_all_started($(APP))." > $(APP)/test/$(APP)_SUITE.erl

	$i "Run Common Test with code coverage enabled"
	$t $(MAKE) -C $(APP) ct COVER=1 $v

	$i "Check that app_one and app_three were covered, but app_two wasn't"
	$t test -f $(APP)/logs/ct_run.*/app_one_app.COVER.html
	$t test -f $(APP)/logs/ct_run.*/app_three_app.COVER.html
	$t ! test -e $(APP)/logs/ct_run.*/app_two_app.COVER.html

cover-ct-incl-apps-default: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a new application app_one"
	$t $(MAKE) -C $(APP) new-app in=app_one $v

	$i "Create a new application app_two"
	$t $(MAKE) -C $(APP) new-app in=app_two $v

	$i "Create a new application app_three"
	$t $(MAKE) -C $(APP) new-app in=app_three $v

	$i "Add all three apps to LOCAL_DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = app_one app_two app_three\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add all three apps to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tapp_one,\n\t\tapp_two,\n\t\tapp_three,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> application:ensure_all_started($(APP))." > $(APP)/test/$(APP)_SUITE.erl

	$i "Run Common Test with code coverage enabled"
	$t $(MAKE) -C $(APP) ct COVER=1 $v

	$i "Check that all apps were covered by default"
	$t test -f $(APP)/logs/ct_run.*/app_one_app.COVER.html
	$t test -f $(APP)/logs/ct_run.*/app_two_app.COVER.html
	$t test -f $(APP)/logs/ct_run.*/app_three_app.COVER.html

cover-ct-incl-deps: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add Cowboy 1.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy_commit = 1.0.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Add Cowboy and Cowlib to the code coverage"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "COVER_DEPS = cowboy cowlib\n"}' $(APP)/Makefile

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> application:ensure_all_started($(APP))." > $(APP)/test/$(APP)_SUITE.erl

	$i "Run Common Test with code coverage enabled"
	$t $(MAKE) -C $(APP) ct COVER=1 $v

	$i "Check that Cowboy and Cowlib were covered, but Ranch wasn't"
	$t test -f $(APP)/logs/ct_run.*/cowboy_app.COVER.html
	$t test -f $(APP)/logs/ct_run.*/cow_http_hd.COVER.html
	$t ! test -e $(APP)/logs/ct_run.*/ranch_app.COVER.html

cover-ct-single-suite: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> application:start($(APP))." > $(APP)/test/$(APP)_SUITE.erl

	$i "Run Common Test against this specific test suite with code coverage enabled"
	$t $(MAKE) -C $(APP) ct-$(APP) COVER=1 $v

	$i "Check that the generated files exist"
	$t test -f $(APP)/cover/ct.coverdata
	$t test -f $(APP)/test/ct.cover.spec

	$i "Check that the generated files are removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/cover/ct.coverdata
	$t test ! -e $(APP)/test/ct.cover.spec

cover-custom-dir: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Set COVER_DATA_DIR in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "COVER_DATA_DIR = custom_dir\n"}' $(APP)/Makefile

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> ok." > $(APP)/test/$(APP)_SUITE.erl

	$i "Run Common Test with code coverage enabled"
	$t $(MAKE) -C $(APP) ct COVER=1 $v

	$i "Run EUnit with code coverage enabled"
	$t $(MAKE) -C $(APP) eunit COVER=1 $v

	$i "Check that the generated file exists"
	$t test -f $(APP)/custom_dir/ct.coverdata
	$t test -f $(APP)/custom_dir/eunit.coverdata

	$i "Merge coverdata files into all.coverdata"
	$t $(MAKE) -C $(APP) all.coverdata $v
	$t test -f $(APP)/custom_dir/all.coverdata

	$i "Check that the generated file is removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/custom_dir/eunit.coverdata
	$t test ! -e $(APP)/custom_dir/ct.coverdata
	$t test ! -e $(APP)/custom_dir/all.coverdata

	$i "Check that the custom dir is removed on distclean"
	$t $(MAKE) -C $(APP) distclean $v
	$t test ! -e $(APP)/custom_dir/

	$i "Check that the custom dir is not removed if not empty"
	$t mkdir $(APP)/custom_dir
	$t touch $(APP)/custom_dir/file
	$t $(MAKE) -C $(APP) distclean $v
	$t test -f $(APP)/custom_dir/file

cover-eunit: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Run EUnit with code coverage enabled"
	$t $(MAKE) -C $(APP) eunit COVER=1 $v

	$i "Check that the generated file exists"
	$t test -f $(APP)/cover/eunit.coverdata

	$i "Check that the generated file is removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/cover/eunit.coverdata

cover-eunit-apps-only: init

	$i "Create a multi application repository with no root application"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t echo "include erlang.mk" > $(APP)/Makefile

	$i "Create a new application named my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Generate a module containing EUnit tests in my_app"
	$t printf "%s\n" \
		"-module(my_app)." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/apps/my_app/src/my_app.erl

	$i "Run EUnit with code coverage enabled"
	$t $(MAKE) -C $(APP) eunit COVER=1 $v

	$i "Check that no file was generated in the top-level directory"
	$t ! test -f $(APP)/cover/eunit.coverdata

	$i "Check that the generated file exists"
	$t test -f $(APP)/apps/my_app/cover/eunit.coverdata

cover-eunit-incl-apps: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a new application app_one"
	$t $(MAKE) -C $(APP) new-app in=app_one $v

	$i "Create a new application app_two"
	$t $(MAKE) -C $(APP) new-app in=app_two $v

	$i "Create a new application app_three"
	$t $(MAKE) -C $(APP) new-app in=app_three $v

	$i "Add all three apps to LOCAL_DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = app_one app_two app_three\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add all three apps to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tapp_one,\n\t\tapp_two,\n\t\tapp_three,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Add app_one and app_three to the code coverage"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "COVER_APPS = app_one app_three\n"}' $(APP)/Makefile

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> application:ensure_all_started($(APP))." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Run EUnit with code coverage enabled"
	$t $(MAKE) -C $(APP) eunit COVER=1 $v

	$i "Build the cover report"
	$t $(MAKE) -C $(APP) cover-report $v

	$i "Check that app_one and app_three were covered, but app_two wasn't"
	$t test -f $(APP)/cover/app_one_app.COVER.html
	$t test -f $(APP)/cover/app_three_app.COVER.html
	$t ! test -e $(APP)/cover/app_two_app.COVER.html

cover-eunit-incl-apps-default: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a new application app_one"
	$t $(MAKE) -C $(APP) new-app in=app_one $v

	$i "Create a new application app_two"
	$t $(MAKE) -C $(APP) new-app in=app_two $v

	$i "Create a new application app_three"
	$t $(MAKE) -C $(APP) new-app in=app_three $v

	$i "Add all three apps to LOCAL_DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "LOCAL_DEPS = app_one app_two app_three\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add all three apps to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tapp_one,\n\t\tapp_two,\n\t\tapp_three,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> application:ensure_all_started($(APP))." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Run EUnit with code coverage enabled"
	$t $(MAKE) -C $(APP) eunit COVER=1 $v

	$i "Build the cover report"
	$t $(MAKE) -C $(APP) cover-report $v

	$i "Check that all apps were covered by default"
	$t test -f $(APP)/cover/app_one_app.COVER.html
	$t test -f $(APP)/cover/app_two_app.COVER.html
	$t test -f $(APP)/cover/app_three_app.COVER.html

cover-eunit-incl-deps: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add Cowboy 1.0.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy_commit = 1.0.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Add Cowboy and Cowlib to the code coverage"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "COVER_DEPS = cowboy cowlib\n"}' $(APP)/Makefile

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> application:ensure_all_started($(APP))." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Run EUnit with code coverage enabled"
	$t $(MAKE) -C $(APP) eunit COVER=1 $v

	$i "Build the cover report"
	$t $(MAKE) -C $(APP) cover-report $v

	$i "Check that Cowboy and Cowlib were covered, but Ranch wasn't"
	$t test -f $(APP)/cover/cowboy_app.COVER.html
	$t test -f $(APP)/cover/cow_http_hd.COVER.html
	$t ! test -e $(APP)/cover/ranch_app.COVER.html

cover-proper: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add PropEr to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = proper\n"}' $(APP)/Makefile

	$i "Generate a module containing Proper properties"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"proper/include/proper.hrl\")." \
		"prop_foo() -> ?FORALL(_, any(), true)." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Run PropEr with code coverage enabled"
	$t $(MAKE) -C $(APP) proper COVER=1 $v

	$i "Check that the generated file exists"
	$t test -f $(APP)/cover/proper.coverdata

	$i "Check that the generated file is removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/cover/proper.coverdata

cover-report-and-merge: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate a Common Test suite"
	$t mkdir $(APP)/test
	$t printf "%s\n" \
		"-module($(APP)_SUITE)." \
		"-export([all/0, ok/1])." \
		"all() -> [ok]." \
		"ok(_) -> ok." > $(APP)/test/$(APP)_SUITE.erl

	$i "Generate a module containing EUnit tests"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"eunit/include/eunit.hrl\")." \
		"ok_test() -> ok." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Run tests with code coverage enabled"
	$t $(MAKE) -C $(APP) tests COVER=1 $v

	$i "Check that the generated files exist"
	$t test -f $(APP)/cover/$(APP).COVER.html
	$t test -f $(APP)/cover/index.html
	$t test -f $(APP)/cover/ct.coverdata
	$t test -f $(APP)/cover/eunit.coverdata
	$t test -f $(APP)/test/ct.cover.spec

	$i "Merge coverdata files into all.coverdata"
	$t $(MAKE) -C $(APP) all.coverdata $v
	$t test -f $(APP)/cover/all.coverdata

	$i "Check that the generated files are removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/cover/all.coverdata
	$t test ! -e $(APP)/cover/ct.coverdata
	$t test ! -e $(APP)/cover/eunit.coverdata
	$t test ! -e $(APP)/test/ct.cover.spec

	$i "Check that the cover report is removed on distclean"
	$t $(MAKE) -C $(APP) distclean $v
	$t test ! -e $(APP)/cover/

cover-triq: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Triq to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = triq\n"}' $(APP)/Makefile

	$i "Generate a module containing Triq properties"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-ifdef(TEST)." \
		"-include_lib(\"triq/include/triq.hrl\")." \
		"prop_foo() -> ?FORALL(_, any(), true)." \
		"-endif." > $(APP)/src/$(APP).erl

	$i "Run Triq with code coverage enabled"
	$t $(MAKE) -C $(APP) triq COVER=1 $v

	$i "Check that the generated file exists"
	$t test -f $(APP)/cover/triq.coverdata

	$i "Check that the generated file is removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/cover/triq.coverdata
