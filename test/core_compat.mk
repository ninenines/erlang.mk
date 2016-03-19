# Core: Compatibility with other build tools.
#
# Note: autopatch functionality is covered separately.

CORE_COMPAT_CASES = auto-rebar rebar rebar-deps-git rebar-deps-hex rebar-deps-pkg rebar-erlc-opts rebar-pt
CORE_COMPAT_TARGETS = $(addprefix core-compat-,$(CORE_COMPAT_CASES))

REBAR_BINARY = https://github.com/rebar/rebar/releases/download/2.6.0/rebar
REBAR3_BINARY = https://s3.amazonaws.com/rebar3/rebar3

.PHONY: core-compat $(CORE_COMPAT_TARGETS)

core-compat: $(CORE_COMPAT_TARGETS)

core-compat-auto-rebar: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add rebar.config as a dependency of 'app' target"
	$t echo "app:: rebar.config" >> $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that rebar.config was created"
	$t test -f $(APP)/rebar.config

	$i "Check that rebar.config can be loaded"
	$t $(ERL) -eval "{ok, _} = file:consult(\"$(APP)/rebar.config\"), halt()"

	$i "Create a temporary file"
	$t touch $(APP)/older_file
	$t $(SLEEP)

	$i "Build the application again"
	$t $(MAKE) -C $(APP) $v

	$i "Check that rebar.config is newer than the temporary file"
	$t test $(APP)/rebar.config -nt $(APP)/older_file

	$i "Check that rebar.config can be loaded"
	$t $(ERL) -eval "{ok, _} = file:consult(\"$(APP)/rebar.config\"), halt()"

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Download rebar"
	$t curl -s -L -o $(APP)/rebar $(REBAR_BINARY)
	$t chmod +x $(APP)/rebar

	$i "Use rebar to build the application"
	$t cd $(APP) && ./rebar compile $v

core-compat-rebar: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Run 'make rebar.config'"
	$t $(MAKE) -C $(APP) rebar.config $v

	$i "Check that rebar.config was created"
	$t test -f $(APP)/rebar.config

	$i "Check that rebar.config can be loaded"
	$t $(ERL) -eval "{ok, _} = file:consult(\"$(APP)/rebar.config\"), halt()"

	$i "Create a temporary file"
	$t touch $(APP)/older_file
	$t $(SLEEP)

	$i "Run 'make rebar.config' again"
	$t $(MAKE) -C $(APP) rebar.config $v

	$i "Check that rebar.config is newer than the temporary file"
	$t test $(APP)/rebar.config -nt $(APP)/older_file

	$i "Check that rebar.config can be loaded"
	$t $(ERL) -eval "{ok, _} = file:consult(\"$(APP)/rebar.config\"), halt()"

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Download rebar"
	$t curl -s -L -o $(APP)/rebar $(REBAR_BINARY)
	$t chmod +x $(APP)/rebar

	$i "Use rebar to build the application"
	$t cd $(APP) && ./rebar compile $v

core-compat-rebar-deps-git: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy as a dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy = git https://github.com/ninenines/cowboy 1.0.0\n"}' $(APP)/Makefile

	$i "Run 'make rebar.config'"
	$t $(MAKE) -C $(APP) rebar.config $v

	$i "Check that rebar.config was created"
	$t test -f $(APP)/rebar.config

	$i "Check that Cowboy is listed in rebar.config"
	$t $(ERL) -eval " \
		{ok, C} = file:consult(\"$(APP)/rebar.config\"), \
		{_, [{cowboy, _, {git, _, \"1.0.0\"}}]} = lists:keyfind(deps, 1, C), \
		halt()"

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Download rebar"
	$t curl -s -L -o $(APP)/rebar $(REBAR_BINARY)
	$t chmod +x $(APP)/rebar

	$i "Use rebar to build the application"
	$t cd $(APP) && ./rebar get-deps compile $v

core-compat-rebar-deps-hex: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy as a dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy = hex 1.0.0\n"}' $(APP)/Makefile

	$i "Run 'make rebar.config'"
	$t $(MAKE) -C $(APP) rebar.config $v

	$i "Check that rebar.config was created"
	$t test -f $(APP)/rebar.config

	$i "Check that Cowboy is listed in rebar.config"
	$t $(ERL) -eval " \
		{ok, C} = file:consult(\"$(APP)/rebar.config\"), \
		{_, [{cowboy, \"1.0.0\"}]} = lists:keyfind(deps, 1, C), \
		halt()"

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Download rebar3"
	$t curl -s -L -o $(APP)/rebar3 $(REBAR3_BINARY)
	$t chmod +x $(APP)/rebar3

	$i "Use rebar to build the application"
	$t cd $(APP) && ./rebar3 compile $v

core-compat-rebar-deps-pkg: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy package as a dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\n"}' $(APP)/Makefile

	$i "Run 'make rebar.config'"
	$t $(MAKE) -C $(APP) rebar.config $v

	$i "Check that rebar.config was created"
	$t test -f $(APP)/rebar.config

	$i "Check that Cowboy is listed in rebar.config"
	$t $(ERL) -eval " \
		{ok, C} = file:consult(\"$(APP)/rebar.config\"), \
		{_, [{cowboy, _, {git, \"https://github.com/\" ++ _, _}}]} = lists:keyfind(deps, 1, C), \
		halt()"

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Download rebar"
	$t curl -s -L -o $(APP)/rebar $(REBAR_BINARY)
	$t chmod +x $(APP)/rebar

	$i "Use rebar to build the application"
	$t cd $(APP) && ./rebar get-deps compile $v

core-compat-rebar-erlc-opts: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add extra options to ERLC_OPTS"
	$t echo "ERLC_OPTS += +warn_export_all +warn_missing_spec +warn_untyped_record" >> $(APP)/Makefile

	$i "Run 'make rebar.config'"
	$t $(MAKE) -C $(APP) rebar.config $v

	$i "Check that rebar.config was created"
	$t test -f $(APP)/rebar.config

	$i "Check that -Werror is not listed in rebar.config"
	$t $(ERL) -eval " \
		{ok, C} = file:consult(\"$(APP)/rebar.config\"), \
		{_, Opts} = lists:keyfind(erl_opts, 1, C), \
		false = lists:member(warning_as_errors, Opts), \
		halt()"

	$i "Check that debug_info is listed in rebar.config"
	$t $(ERL) -eval " \
		{ok, C} = file:consult(\"$(APP)/rebar.config\"), \
		{_, Opts} = lists:keyfind(erl_opts, 1, C), \
		true = lists:member(debug_info, Opts), \
		halt()"

	$i "Check that extra options are listed in rebar.config"
	$t $(ERL) -eval " \
		{ok, C} = file:consult(\"$(APP)/rebar.config\"), \
		{_, Opts} = lists:keyfind(erl_opts, 1, C), \
		true = lists:member(warn_export_all, Opts), \
		true = lists:member(warn_missing_spec, Opts), \
		true = lists:member(warn_untyped_record, Opts), \
		halt()"

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Download rebar"
	$t curl -s -L -o $(APP)/rebar $(REBAR_BINARY)
	$t chmod +x $(APP)/rebar

	$i "Use rebar to build the application"
	$t cd $(APP) && ./rebar compile $v

core-compat-rebar-pt: build clean

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Generate .erl files"
	$t echo "-module(boy)." > $(APP)/src/boy.erl
	$t echo "-module(girl)." > $(APP)/src/girl.erl

	$i "Add lager to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager\n"}' $(APP)/Makefile

	$i "Add the lager_transform parse_transform to ERLC_OPTS"
	$t echo "ERLC_OPTS += +'{parse_transform, lager_transform}' +'{lager_truncation_size, 1234}'" >> $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Run 'make rebar.config'"
	$t $(MAKE) -C $(APP) rebar.config $v

	$i "Check that rebar.config was created"
	$t test -f $(APP)/rebar.config

	$i "Check that the parse_transform option is listed in rebar.config"
	$t $(ERL) -eval " \
		{ok, C} = file:consult(\"$(APP)/rebar.config\"), \
		{_, Opts} = lists:keyfind(erl_opts, 1, C), \
		true = lists:member({parse_transform, lager_transform}, Opts), \
		true = lists:member({lager_truncation_size, 1234}, Opts), \
		halt()"

# For the new build method, we have to simulate keeping the .app file
# inside the repository, by leaving it in the ebin/ directory before
# calling Rebar.
ifndef LEGACY
	$i "Move the .app file outside ebin/"
	$t mv $(APP)/ebin/$(APP).app $(APP)/
endif

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

ifndef LEGACY
	$i "Put the .app file back into ebin/"
	$t mkdir $(APP)/ebin/
	$t mv $(APP)/$(APP).app $(APP)/ebin/
endif

	$i "Download rebar"
	$t curl -s -L -o $(APP)/rebar $(REBAR_BINARY)
	$t chmod +x $(APP)/rebar

	$i "Use rebar to build the application"
	$t cd $(APP) && ./rebar get-deps compile $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/boy.beam
	$t test -f $(APP)/ebin/girl.beam
