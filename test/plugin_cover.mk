# Common Test plugin.

COVER_CASES = ct custom-dir eunit report-and-merge
COVER_TARGETS = $(addprefix cover-,$(COVER_CASES))

.PHONY: cover $(COVER_TARGETS)

cover: $(COVER_TARGETS)

cover-ct: build clean

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

	$i "Run Common Test with code coverage enabled"
	$t $(MAKE) -C $(APP) ct COVER=1 $v

	$i "Check that the generated files exist"
	$t test -f $(APP)/ct.coverdata
	$t test -f $(APP)/test/ct.cover.spec

	$i "Check that the generated files are removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/ct.coverdata
	$t test ! -e $(APP)/test/ct.cover.spec

cover-custom-dir: build clean

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

cover-eunit: build clean

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
	$t test -f $(APP)/eunit.coverdata

	$i "Check that the generated file is removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/eunit.coverdata

cover-report-and-merge: build clean

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
	$t test -f $(APP)/ct.coverdata
	$t test -f $(APP)/eunit.coverdata
	$t test -f $(APP)/test/ct.cover.spec

	$i "Merge coverdata files into all.coverdata"
	$t $(MAKE) -C $(APP) all.coverdata $v
	$t test -f $(APP)/all.coverdata

	$i "Check that the generated files are removed on clean"
	$t $(MAKE) -C $(APP) clean $v
	$t test ! -e $(APP)/all.coverdata
	$t test ! -e $(APP)/ct.coverdata
	$t test ! -e $(APP)/eunit.coverdata
	$t test ! -e $(APP)/test/ct.cover.spec

	$i "Check that the cover report is removed on distclean"
	$t $(MAKE) -C $(APP) distclean $v
	$t test ! -e $(APP)/cover/
