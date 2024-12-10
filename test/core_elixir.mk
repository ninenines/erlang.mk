# Core: Miscellaneous.
#
# The miscellaneous tests use the prefix "core-", not "core-misc-".

CORE_ELIXIR_TARGETS = $(call list_targets,core-elixir)

.PHONY: core-elixir $(CORE_ELIXIR_TARGETS)

core-elixir: $(CORE_ELIXIR_TARGETS)

core-elixir-from-dep: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Elixir, Lager, Jason, Phoenix to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = elixir lager jason phoenix\ndep_elixir_commit = v1.17.3\ndep_lager = git https://github.com/erlang-lager/lager master\ndep_jason = git https://github.com/michalmuskala/jason.git master\ndep_phoenix = hex 1.7.2\n"}' $(APP)/Makefile

	$i "Add the lager_transform parse_transform to ERLC_OPTS"
	$t echo "ERLC_OPTS += +'{parse_transform, lager_transform}'" >> $(APP)/Makefile

ifdef LEGACY
	$i "Add Elixir, Lager, Jason and Phoenix to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\telixir,\n\t\tlager,\n\t\tjason,\n\t\tphoenix,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched and built"
	$t test -f $(APP)/deps/elixir/ebin/dep_built
	$t test -f $(APP)/deps/lager/ebin/dep_built
	$t test -f $(APP)/deps/jason/ebin/dep_built
	$t test -f $(APP)/deps/phoenix/ebin/dep_built

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -pa $(APP)/deps/*/ebin -pa $(APP)/deps/elixir/lib/*/ebin -eval " \
		{ok, Apps} = application:ensure_all_started('$(APP)'), \
		true = lists:member(elixir, Apps), \
		true = lists:member(lager, Apps), \
		true = lists:member(jason, Apps), \
		true = lists:member(phoenix, Apps), \
		halt()"

	$i "Check that the Jason application depends on Elixir builtins"
	$t $(ERL) -pa $(APP)/ebin/ -pa $(APP)/deps/*/ebin -pa $(APP)/deps/elixir/lib/*/ebin -eval " \
		{ok, Apps} = application:ensure_all_started(jason), \
		true = lists:member(elixir, Apps), \
		true = lists:member(eex, Apps), \
		true = lists:member(logger, Apps), \
		true = lists:member(mix, Apps), \
		halt()"

core-elixir-from-system: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Lager, Jason, Phoenix to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager jason phoenix\ndep_lager = git https://github.com/erlang-lager/lager master\ndep_jason = git https://github.com/michalmuskala/jason.git master\ndep_phoenix = hex 1.7.2\n"}' $(APP)/Makefile

	$i "Add the lager_transform parse_transform to ERLC_OPTS"
	$t echo "ERLC_OPTS += +'{parse_transform, lager_transform}'" >> $(APP)/Makefile

ifdef LEGACY
	$i "Add Lager, Jason and Phoenix to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\telixir,\n\t\tlager,\n\t\tjason,\n\t\tphoenix,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all dependencies were fetched and built"
	$t ! test -e $(APP)/deps/elixir
	$t test -f $(APP)/deps/lager/ebin/dep_built
	$t test -f $(APP)/deps/jason/ebin/dep_built
	$t test -f $(APP)/deps/phoenix/ebin/dep_built

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -pa $(APP)/deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
		{ok, Apps} = application:ensure_all_started('$(APP)'), \
		true = lists:member(lager, Apps), \
		true = lists:member(jason, Apps), \
		true = lists:member(phoenix, Apps), \
		halt()"

	$i "Check that the Jason application depends on Elixir builtins"
	$t $(ERL) -pa $(APP)/ebin/ -pa $(APP)/deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
		{ok, Apps} = application:ensure_all_started(jason), \
		true = lists:member(elixir, Apps), \
		true = lists:member(eex, Apps), \
		true = lists:member(logger, Apps), \
		true = lists:member(mix, Apps), \
		halt()"

core-elixir-nif: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Libsalty2 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = libsalty2\ndep_libsalty2 = git https://github.com/Ianleeclark/libsalty2.git b11e544\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Libsalty2 to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tlibsalty2,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -pa $(APP)/deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
		{ok, Apps} = application:ensure_all_started('$(APP)'), \
		true = lists:member(libsalty2, Apps), \
		halt()"

core-elixir-rel: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib bootstrap-rel $v

	$i "Add Lager, Jason, Phoenix to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager jason phoenix\ndep_lager = git https://github.com/erlang-lager/lager master\ndep_jason = git https://github.com/michalmuskala/jason.git master\ndep_phoenix = hex 1.7.2\n"}' $(APP)/Makefile

	$i "Add the lager_transform parse_transform to ERLC_OPTS"
	$t echo "ERLC_OPTS += +'{parse_transform, lager_transform}'" >> $(APP)/Makefile

ifdef LEGACY
	$i "Add Lager, Jason and Phoenix to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tlager,\n\t\tjason,\n\t\tphoenix,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the release was built"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/1
