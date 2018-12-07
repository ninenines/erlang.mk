# Core: Autopatch.

CORE_AUTOPATCH_TARGETS = $(call list_targets,core-autopatch)

.PHONY: core-autopatch $(CORE_AUTOPATCH_TARGETS)

core-autopatch: $(CORE_AUTOPATCH_TARGETS)

core-autopatch-extended: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Ranch to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = ranch\n"}' $(APP)/Makefile

	$i "Extend autopatch-ranch to create an additional module"
	$t echo "autopatch-ranch:: ; rm -f \$$(DEPS_DIR)/ranch/src/ranch_protocol.erl" >> $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the module was removed"
	$t ! test -e $(APP)/deps/ranch/src/ranch_protocol.erl
	$t ! test -e $(APP)/deps/ranch/ebin/ranch_protocol.beam

core-autopatch-extended-erlc-opts: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add couchbeam to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = couchbeam\n"}' $(APP)/Makefile

	$i "Extend autopatch-couchbeam to add options to its ERLC_OPTS"
	$t echo "autopatch-couchbeam:: ; printf '\nERLC_OPTS += -DWITH_JIFFY\n' >> \$$(DEPS_DIR)/couchbeam/Makefile" >> $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that couchbeam_ejson was compiled with the added option"
	$t $(ERL) -pa $(APP)/deps/couchbeam/ebin -eval 'c:m(couchbeam_ejson), halt()' | grep -c "WITH_JIFFY" | grep -q 1

core-autopatch-no-autopatch: init

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

core-autopatch-no-autopatch-erlang-mk: init

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

core-autopatch-no-autopatch-rebar: init

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

core-autopatch-port_env: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Bootstrap a new NIF named my_dep inside $(APP) that uses rebar"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap-lib $v
	$t $(MAKE) -C $(APP)/my_dep/ new-nif n=my_dep $v
	$t rm $(APP)/my_dep/erlang.mk $(APP)/my_dep/Makefile

	$i "Add a rebar.config file with port_env to my_dep"
	$t echo "{port_env, [" >> $(APP)/my_dep/rebar.config
	$t echo "{\"CFLAGS\", \"\$$CFLAGS \$$(pkg-config --cflags domain-classifier 2>/dev/null)\"}," >> $(APP)/my_dep/rebar.config
	$t echo "{\"LDFLAGS\", \"\$$LDFLAGS \$$(pkg-config --libs domain-classifier 2>/dev/null)\"}" >> $(APP)/my_dep/rebar.config
	$t echo "]}." >> $(APP)/my_dep/rebar.config

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

	$i "Confirm that the port_env configuration was expanded properly"
	$t grep -q "shell pkg-config" $(APP)/deps/my_dep/c_src/Makefile.erlang.mk

core-autopatch-rebar: init

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

# This test is expected to fail when run in parallel and flock/lockf is not available.
core-autopatch-two-rebar: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add two Rebar projects to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = epgsql mochiweb\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v
