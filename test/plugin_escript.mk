# Escript plugin.

ESCRIPT_CASES = build deps distclean extra
ESCRIPT_TARGETS = $(addprefix escript-,$(ESCRIPT_CASES))

.PHONY: escript $(ESCRIPT_TARGETS)

escript: $(ESCRIPT_TARGETS)

escript-build: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate a module containing a function main/1"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-export([main/1])." \
		'main(_) -> io:format("good~n").' > $(APP)/src/$(APP).erl

	$i "Build the escript"
	$t $(MAKE) -C $(APP) escript $v

	$i "Check that the escript exists"
	$t test -f $(APP)/$(APP)

	$i "Check that the escript runs"
	$t $(APP)/$(APP) | grep -q good

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that the escript was removed"
	$t test ! -e $(APP)/$(APP)

escript-deps: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Ranch to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = ranch\n"}' $(APP)/Makefile

	$i "Generate a module containing a function main/1"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-export([main/1])." \
		'main(_) -> io:format("good~n").' > $(APP)/src/$(APP).erl

	$i "Build the escript"
	$t $(MAKE) -C $(APP) escript $v

	$i "Check that the escript runs"
	$t $(APP)/$(APP) | grep -q good

	$i "Check that the escript contains the dependency"
	$t zipinfo $(APP)/$(APP) 2> /dev/null | grep -q ranch

escript-distclean: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Use a non-standard name for the escript"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "ESCRIPT_FILE = real-escript\n"}' $(APP)/Makefile

	$i "Generate a module containing a function main/1"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-export([main/1])." \
		'main(_) -> io:format("good~n").' > $(APP)/src/$(APP).erl

	$i "Build the escript"
	$t $(MAKE) -C $(APP) escript $v

	$i "Check that the escript runs"
	$t test ! -f $(APP)/$(APP)
	$t $(APP)/real-escript | grep -q good

	$i "Check that make distclean removes the generated escript"
	$t $(MAKE) -C $(APP) distclean $v
	$t test ! -f $(APP)/$(APP)
	$t test ! -f $(APP)/real-escript

escript-extra: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Instruct Erlang.mk to add extra files to the escript"
	$t printf "%s\n" \
		"escript-zip::" \
		'	$$(verbose) $$(ESCRIPT_ZIP) $$(ESCRIPT_ZIP_FILE) Makefile erlang.mk' >> $(APP)/Makefile

	$i "Generate a module containing a function main/1"
	$t printf "%s\n" \
		"-module($(APP))." \
		"-export([main/1])." \
		'main(_) -> io:format("good~n").' > $(APP)/src/$(APP).erl

	$i "Build the escript"
	$t $(MAKE) -C $(APP) escript $v

	$i "Check that the escript runs"
	$t $(APP)/$(APP) | grep -q good

	$i "Check that the escript contains the extra files"
	$t zipinfo $(APP)/$(APP) 2> /dev/null | grep -q Makefile
	$t zipinfo $(APP)/$(APP) 2> /dev/null | grep -q erlang.mk
