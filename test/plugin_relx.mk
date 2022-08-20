# Relx plugin.
#
# Sleeps when interacting with relx script are necessary after start and upgrade
# as both of those interactions are not synchronized.

RELX_TARGETS = $(call list_targets,relx)

.PHONY: relx $(RELX_TARGETS)

ifeq ($(PLATFORM),msys2)
RELX_REL_EXT = .cmd
else
RELX_REL_EXT =
endif

relx: $(RELX_TARGETS)

relx-rel: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the release was built"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/1

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that the release still exists"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/1

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that the output directory was removed entirely"
	$t test ! -d $(APP)/_rel/

relx-apps-with-deps: init

	$i "Bootstrap a new release as a multi application repository"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib bootstrap-rel $v

	$i "Create a new application my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Include my_app in the release"
	$t perl -pi.bak -e 's/$(APP),/$(APP), my_app,/' $(APP)/relx.config

	$i "Add Cowlib to the list of dependencies for my_app"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\n"}' $(APP)/apps/my_app/Makefile

ifdef LEGACY
	$i "Add Cowlib to the applications key in the my_app.app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowlib,\n"}' $(APP)/apps/my_app/src/my_app.app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/apps/my_app/my_app.d
	$t test -f $(APP)/apps/my_app/ebin/my_app.app
	$t test -f $(APP)/apps/my_app/ebin/my_app_app.beam
	$t test -f $(APP)/apps/my_app/ebin/my_app_sup.beam
	$t test -f $(APP)/deps/cowlib/ebin/cowlib.app

	$i "Check that Cowlib was included in the release"
	$t test -d $(APP)/_rel/test_relx_apps_with_deps_release/lib/cowlib-1.0.2

relx-bare-rel: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Build the release"
	$t $(MAKE) -C $(APP) rel $v

	$i "Check that the release was built"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/1

relx-post-rel: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Add relx-post-rel target to Makefile"
	$t echo "relx-post-rel::" >> $(APP)/Makefile
	$t echo "	echo test post rel > _rel/$(APP)_release/test_post_rel" >> $(APP)/Makefile

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the release was built"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/1
	$t test -f $(APP)/_rel/$(APP)_release/test_post_rel
	$t test "test post rel" = "`cat $(APP)/_rel/$(APP)_release/test_post_rel`"

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that the release still exists"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/1
	$t test -f $(APP)/_rel/$(APP)_release/test_post_rel
	$t test "test post rel" = "`cat $(APP)/_rel/$(APP)_release/test_post_rel`"

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that the output directory was removed entirely"
	$t test ! -d $(APP)/_rel/

relx-rel-with-script: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Create a relx.config.script file"
	$t printf "%s\n" \
		"{release, {App, _Ver}, Apps} = lists:keyfind(release, 1, CONFIG)," \
		"lists:keyreplace(release, 1, CONFIG, {release, {App, \"ONE\"}, Apps})." \
		> $(APP)/relx.config.script

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the release was built"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/ONE

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that the release still exists"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/ONE

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that the output directory was removed entirely"
	$t test ! -d $(APP)/_rel/

define relx-rel-with-only-script-relx.config.script.erl
endef

relx-rel-with-script-only: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Delete relx.config and create a relx.config.script file"
	$t rm -f $(APP)/relx.config
	$t printf "%s\n" \
		"CONFIG = [], %% Assert that config is empty." \
		"[" \
		"	{release, {$(APP)_release, \"ONE\"}, [$(APP), sasl, runtime_tools]}," \
		"	{dev_mode, false}," \
		"	{include_erts, true}," \
		"	{extended_start_script, true}," \
		"	{sys_config, \"config/sys.config\"}," \
		"	{vm_args, \"config/vm.args\"}" \
		"| CONFIG]." \
		> $(APP)/relx.config.script

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the release was built"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/ONE

	$i "Clean the application"
	$t $(MAKE) -C $(APP) clean $v

	$i "Check that the release still exists"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/ONE

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v

	$i "Check that the output directory was removed entirely"
	$t test ! -d $(APP)/_rel/

ifneq ($(PLATFORM),msys2)
# This test is currently disabled on Windows because we are
# running into too many issues preventing the test from
# executing properly and leaving the release running at the end.
relx-relup: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Set the initial application version"
ifeq ($(LEGACY),1)
	$t sed -i.bak s/"{vsn, \"0.1.0\"}"/"{vsn, \"1\"}"/ $(APP)/src/$(APP).app.src
else
	$t echo "PROJECT_VERSION = 1" >> $(APP)/Makefile
endif

	$i "Generate a test module"
	$t printf "%s\n"\
		"-module(test)." \
		"-export([test/0])." \
		"test() -> old." > $(APP)/src/test.erl

	$i "Build the initial release as a tarball"
	$t $(MAKE) -C $(APP) $v

	$i "Update the test module"
	$t sed -i.bak s/"test() -> old."/"test() -> new."/ $(APP)/src/test.erl

	$i "Bump the application version"
ifeq ($(LEGACY),1)
	$t sed -i.bak s/"{vsn, \"1\"}"/"{vsn, \"2\"}"/ $(APP)/src/$(APP).app.src
else
	$t sed -i.bak s/"PROJECT_VERSION = 1"/"PROJECT_VERSION = 2"/ $(APP)/Makefile
endif

	$i "Generate a .appup for the application"
	$t printf "%s\n" \
		"{\"2\","\
		"  [{\"1\", [{load_module, test}]}],"\
		"  [{\"1\", [{load_module, test}]}]"\
		"}." > $(APP)/ebin/$(APP).appup

	$i "Bump the release version"
	$t sed -i.bak s/"1"/"2"/ $(APP)/relx.config

	$i "Build a new release with a relup as a tarball"
	$t $(MAKE) -C $(APP) relup $v

	$i "Test that both releases are available"
	$t test -f $(APP)/_rel/$(APP)_release/$(APP)_release-1.tar.gz
	$t test -f $(APP)/_rel/$(APP)_release/$(APP)_release-2.tar.gz

	$i "Unpack initial release"
	$t mkdir $(APP)/tmp
	$t tar -xzf $(APP)/_rel/$(APP)_release/$(APP)_release-1.tar.gz -C $(APP)/tmp

ifeq ($(PLATFORM),msys2)
	$i "Start initial release"
	$t $(APP)/tmp/bin/$(APP)_release$(RELX_REL_EXT) install
else
	$i "Start initial release and confirm it runs the old code"
endif
	$t $(APP)/tmp/bin/$(APP)_release$(RELX_REL_EXT) daemon
	$t sleep 1
ifneq ($(PLATFORM),msys2)
# On Windows the script does not have the commands rpc and versions.
	$t test `$(APP)/tmp/bin/$(APP)_release$(RELX_REL_EXT) rpc test test` = old

	$i "Check that it's 1 available version"
	$t test `$(APP)/tmp/bin/$(APP)_release$(RELX_REL_EXT) versions | wc -l` = "2"
endif

	$i "Copy the relup tarball to the release directory"
	$t mkdir $(APP)/tmp/releases/2
	$t cp $(APP)/_rel/$(APP)_release/$(APP)_release-2.tar.gz $(APP)/tmp/releases/2/$(APP)_release.tar.gz
	$t test -f $(APP)/tmp/releases/2/$(APP)_release.tar.gz

ifeq ($(PLATFORM),msys2)
	$i "Upgrade the release"
# On Windows the script doesn't seem to change the cwd properly
# which results in the release tarball not being found.
#
# We use --no-permanent to avoid another bug.
	$t cd $(APP)/tmp && ./bin/$(APP)_release$(RELX_REL_EXT) upgrade --no-permanent "2"
else
	$i "Upgrade the release and confirm it runs the new code"
	$t $(APP)/tmp/bin/$(APP)_release$(RELX_REL_EXT) upgrade "2"
endif
	$t sleep 1
ifneq ($(PLATFORM),msys2)
# On Windows the script does not have the commands rpc and versions.
	$t test `$(APP)/tmp/bin/$(APP)_release$(RELX_REL_EXT) rpc test test` = new

	$i "Check that it's 2 available versions"
	$t test `$(APP)/tmp/bin/$(APP)_release$(RELX_REL_EXT) versions | wc -l` = "3"
endif

ifeq ($(PLATFORM),msys2)
	$i "Downgrade the release"
# On Windows the script doesn't seem to change the cwd properly
# which results in the release tarball not being found.
#
# We use --no-permanent to avoid another bug.
	$t cd $(APP)/tmp && ./bin/$(APP)_release$(RELX_REL_EXT) downgrade --no-permanent "1"
else
	$i "Downgrade the release and confirm it runs the old code"
	$t $(APP)/tmp/bin/$(APP)_release$(RELX_REL_EXT) downgrade "1"
endif
	$t sleep 1
ifneq ($(PLATFORM),msys2)
# On Windows the script does not have the commands rpc and versions.
	$t test `$(APP)/tmp/bin/$(APP)_release$(RELX_REL_EXT) rpc test test` = old
endif

	$i "Stop the release"
# On Windows this fails with the following reason:
#   The service test_relx_relup_release_2 is not an erlsrv controlled service.
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) stop
ifeq ($(PLATFORM),msys2)
	$t sleep 1
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) uninstall
endif
endif

relx-start-stop: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Stop the release (in case one is running from a previously aborted run)"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) stop || true
ifeq ($(PLATFORM),msys2)
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) uninstall || true
endif

	$i "Start the release"
ifeq ($(PLATFORM),msys2)
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) install
endif
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) daemon

	$i "Ping the release"
	$t $(call wait_for_success,$(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) ping)

	$i "Stop the release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) stop
ifeq ($(PLATFORM),msys2)
	$t sleep 1
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) uninstall
endif

ifneq ($(PLATFORM),msys2)
# The script will not return false on Windows when the ping fails.
# It sometimes also gets stuck. So we just skip the ping for now.
	$i "Check that further pings get no replies"
	$t $(call wait_for_failure,$(APP)/_rel/$(APP)_release/bin/$(APP)_release$(RELX_REL_EXT) ping)
endif

	$i "Check that there's no erl_crash.dump file"
	$t test ! -f $(APP)/_rel/$(APP)_release/erl_crash.dump

relx-tar: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Build the release without a tarball"
	$t $(MAKE) -C $(APP) RELX_TAR=0 $v

	$i "Check that tarball doesn't exist"
	$t test ! -e $(APP)/_rel/$(APP)_release/$(APP)_release-1.tar.gz

	$i "Build the release as a tarball"
	$t $(MAKE) -C $(APP) $v

	$i "Check that tarball exists"
	$t test -f $(APP)/_rel/$(APP)_release/$(APP)_release-1.tar.gz

relx-vsn-cmd: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Replace the vsn"
	$t sed -i.bak s/"\"1\""/"{cmd, \"printf 2\"}"/ $(APP)/relx.config

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the correct release exists"
	$t ! test -d $(APP)/_rel/$(APP)_release/releases/1
	$t test -d $(APP)/_rel/$(APP)_release/releases/2

relx-vsn-git-long: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Replace the vsn"
	$t sed -i.bak s/"\"1\""/"{git, long}"/ $(APP)/relx.config

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the correct release exists"
	$t ! test -d $(APP)/_rel/$(APP)_release/releases/1
	$t test -d $(APP)/_rel/$(APP)_release/releases/$(shell git rev-parse HEAD)

relx-vsn-git-short: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Replace the vsn"
	$t sed -i.bak s/"\"1\""/"{git, short}"/ $(APP)/relx.config

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the correct release exists"
	$t ! test -d $(APP)/_rel/$(APP)_release/releases/1
	$t test -d $(APP)/_rel/$(APP)_release/releases/$(shell git rev-parse --short HEAD)
