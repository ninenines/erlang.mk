# Core: Querying dependencies.

CORE_QUERY_TARGETS = $(call list_targets,core-query)

.PHONY: core-query $(CORE_QUERY_TARGETS)

core-query: $(CORE_QUERY_TARGETS)

core-query-deps: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.7.0 to DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy_commit = 2.7.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Query the dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): cowboy git https://github.com/ninenines/cowboy 2.7.0" \
		"cowboy: cowlib git https://github.com/ninenines/cowlib 2.8.0" \
		"cowboy: ranch git https://github.com/ninenines/ranch 1.7.1" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-deps.log

core-query-deps-c-lz4: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add lz4 to the list of build dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = lz4_src\ndep_lz4_src = git https://github.com/lz4/lz4 v1.8.2\n"}' $(APP)/Makefile

	$i "Query the dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): lz4_src git https://github.com/lz4/lz4 v1.8.2" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-deps.log

core-query-deps-extra: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add uuid 1.8.0 to DEPS via hex"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = uuid\ndep_uuid = hex 1.8.0 uuid_erl\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add uuid to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tuuid,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Query the dependencies of $(APP) with the extra option"
	$t $(MAKE) -C $(APP) query-deps QUERY="name fetch_method repo version extra" $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): uuid hex https://hex.pm/packages/uuid_erl 1.8.0 package-name=uuid_erl" \
		"uuid: quickrand git https://github.com/okeuday/quickrand.git v1.8.0 -" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-deps.log

core-query-deps-hex: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add cowboy 2.7.0 to DEPS via hex"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy = hex 2.7.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Query the dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): cowboy hex https://hex.pm/packages/cowboy 2.7.0" \
		"cowboy: cowlib git https://github.com/ninenines/cowlib 2.8.0" \
		"cowboy: ranch git https://github.com/ninenines/ranch 1.7.1" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-deps.log

core-query-deps-hex-hex: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add observer_cli 1.5.3 to DEPS via hex"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = observer_cli\ndep_observer_cli = hex 1.5.3\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add observer_cli to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tobserver_cli,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Query the dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): observer_cli hex https://hex.pm/packages/observer_cli 1.5.3" \
		"observer_cli: recon hex https://hex.pm/packages/recon 2.5.0" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-deps.log

core-query-deps-no-duplicates: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.7.0 and Farwest master to DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy farwest\ndep_cowboy_commit = 2.7.0\ndep_farwest = git https://github.com/ninenines/farwest 017dc36b59f028e7014bad927e745a2c0b529018\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy and Farwest to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\t\tfarwest,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Query the dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): cowboy git https://github.com/ninenines/cowboy 2.7.0" \
		"$(APP): farwest git https://github.com/ninenines/farwest 017dc36b59f028e7014bad927e745a2c0b529018" \
		"cowboy: cowlib git https://github.com/ninenines/cowlib 2.8.0" \
		"cowboy: ranch git https://github.com/ninenines/ranch 1.7.1" \
		"farwest: cowlib git https://github.com/ninenines/cowlib master" \
		"farwest: cowboy git https://github.com/ninenines/cowboy master" \
		"farwest: gun git https://github.com/ninenines/gun master" \
		"gun: cowlib git https://github.com/ninenines/cowlib 2.12.1" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-deps.log

core-query-deps-opts: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.7.0 to DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy_commit = 2.7.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Query the dependencies of $(APP) with all options"
	$t $(MAKE) -C $(APP) query-deps QUERY="name fetch_method repo version extra absolute_path" $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): cowboy git https://github.com/ninenines/cowboy 2.7.0 - $(CURDIR)/$(APP)/deps/cowboy" \
		"cowboy: cowlib git https://github.com/ninenines/cowlib 2.8.0 - $(CURDIR)/$(APP)/deps/cowlib" \
		"cowboy: ranch git https://github.com/ninenines/ranch 1.7.1 - $(CURDIR)/$(APP)/deps/ranch" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-deps.log

core-query-deps-opts-in-makefile: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.7.0 to DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy_commit = 2.7.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowboy to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowboy,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Define the QUERY variable with all options in $(APP)'s Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "QUERY = name fetch_method repo version extra absolute_path\n"}' $(APP)/Makefile

	$i "Query the dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): cowboy git https://github.com/ninenines/cowboy 2.7.0 - $(CURDIR)/$(APP)/deps/cowboy" \
		"cowboy: cowlib git https://github.com/ninenines/cowlib 2.8.0 - $(CURDIR)/$(APP)/deps/cowlib" \
		"cowboy: ranch git https://github.com/ninenines/ranch 1.7.1 - $(CURDIR)/$(APP)/deps/ranch" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-deps.log

core-query-doc-deps: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.7.0 to DOC_DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DOC_DEPS = cowboy\ndep_cowboy_commit = 2.7.0\n"}' $(APP)/Makefile

	$i "Query the documentation dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-doc-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): cowboy git https://github.com/ninenines/cowboy 2.7.0" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-doc-deps.log

core-query-no-deps: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Query the dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-deps $v

	$i "Confirm that nothing was found"
	$t touch $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-deps.log

core-query-rel-deps: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.7.0 to REL_DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "REL_DEPS = cowboy\ndep_cowboy_commit = 2.7.0\n"}' $(APP)/Makefile

	$i "Query the release dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-rel-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): cowboy git https://github.com/ninenines/cowboy 2.7.0" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-rel-deps.log

core-query-shell-deps: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.7.0 to SHELL_DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "SHELL_DEPS = cowboy\ndep_cowboy_commit = 2.7.0\n"}' $(APP)/Makefile

	$i "Query the shell dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-shell-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): cowboy git https://github.com/ninenines/cowboy 2.7.0" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-shell-deps.log

core-query-test-deps: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.7.0 to TEST_DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "TEST_DEPS = cowboy\ndep_cowboy_commit = 2.7.0\n"}' $(APP)/Makefile

	$i "Query the test dependencies of $(APP)"
	$t $(MAKE) -C $(APP) query-test-deps $v

	$i "Confirm that the expected applications were found"
	$t printf "%s\n" \
		"$(APP): cowboy git https://github.com/ninenines/cowboy 2.7.0" \
		> $(APP)/expected-deps.txt
	$t cmp $(APP)/expected-deps.txt $(APP)/.erlang.mk/query-test-deps.log
