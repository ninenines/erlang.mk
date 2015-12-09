# relx plugin.
#
# sleeps when interacting with relx script are necessary after start and upgrade
# as both of those interactions are not synchronized

RELX_CASES = rel run rel-tar distclean-relx distclean-relx-rel relup
RELX_TARGETS = $(addprefix relx-,$(RELX_CASES))
RELX_CLEAN_TARGETS = $(addprefix clean-,$(RELX_TARGETS))

APP = $(subst -,_,$@)
APP_TO_CLEAN = $(subst -,_,$(patsubst clean-%,%,$@))

relx: $(RELX_TARGETS)
clean-relx: $(RELX_CLEAN_TARGETS)

$(RELX_CLEAN_TARGETS):
	$t rm -rf $(APP_TO_CLEAN)/

define bootstrap_helper
	$i "Bootstrap a new relx release named $(1)"
	$t mkdir $(1)/
	$t cp ../erlang.mk $(1)/
	$t $(MAKE) -C $(1) -f erlang.mk bootstrap bootstrap-rel $v
endef

relx-rel: clean-relx-rel
	$(call bootstrap_helper,$(APP))

	$i "Build and create release via relx"
	$t $(MAKE) -C $(APP)

	$i "Test if relx got downloaded correctly"
	$t test -f $(APP)/relx

	$i "Check if release directories got generated as expected"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/1

	$i "Clean application"
	$(MAKE) -C $(APP) clean

	$i "Check that _rel directory was not affected"
	$t test -d $(APP)/_rel
	$t test -d $(APP)/_rel/$(APP)_release
	$t test -d $(APP)/_rel/$(APP)_release/bin
	$t test -d $(APP)/_rel/$(APP)_release/lib
	$t test -d $(APP)/_rel/$(APP)_release/releases
	$t test -d $(APP)/_rel/$(APP)_release/releases/1

relx-run: clean-relx-run
	$t $(call bootstrap_helper,$(APP))

	$i "Build and create release via relx"
	$t $(MAKE) -C $(APP)

ifeq ($(PLATFORM),msys2)
	$i "Install and start release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd install $v
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd start $v
	$i "Sleeping for 1 second"
	$t sleep 1
	$i "Ping release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd ping $v
	$i "Stop and uninstall release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd stop $v
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd uninstall $v
	$i "Ping release, expect failure"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd ping $v; test $$? -eq 1
else
	$i "Start release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release start $v
	$i "Sleeping for 1 second"
	$t sleep 1
	$i "Ping release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release ping $v
	$i "Stop release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release stop $v
	$i "Ping release, expect failure"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release ping $v; test $$? -eq 1
endif

relx-rel-tar: clean-relx-rel-tar
	$(call bootstrap_helper,$(APP))

	$i "Build and create release tarball via relx"
	$t $(MAKE) RELX_OPTS="release tar" -C $(APP)

	$i "Test if tar file got created"
	$t test -f $(APP)/_rel/$(APP)_release/$(APP)_release-1.tar.gz

relx-distclean-relx: clean-relx-distclean-relx
	$(call bootstrap_helper,$(APP))

	$i "Build and create release via relx"
	$t $(MAKE) -C $(APP)

	$i "Test if relx got downloaded correctly"
	$t test -f $(APP)/relx

	$i "Run distclean-relx"
	$t $(MAKE) -C $(APP) distclean-relx

	$i "Test if relx got removed"
	$t test ! -f $(APP)/relx

relx-distclean-relx-rel: clean-relx-distclean-relx-rel
	$(call bootstrap_helper,$(APP))

	$i "Build and create release via relx"
	$t $(MAKE) -C $(APP)

	$i "Test if _rel directory got created"
	$t test -d $(APP)/_rel/

	$i "Run distclean-relx-rel"
	$t $(MAKE) -C $(APP) distclean-relx-rel

	$i "Test if _rel directory got removed"
	$t test ! -d $(APP)/_rel/

relx-relup: clean-relx-relup
	$(call bootstrap_helper,$(APP))

	$i "Set version of application"
ifeq ($(LEGACY),1)
	$t sed -i s/"{vsn, \"0.1.0\"}"/"{vsn, \"1\"}"/ $(APP)/src/$(APP).app.src
else
	$t echo "PROJECT_VERSION = 1" >> $(APP)/Makefile
endif

	$i "Include sasl application"
	$t sed -i s/"\[$(APP)\]"/"\[$(APP),sasl\]"/ -i $(APP)/relx.config

	$i "Add test module"
	$t printf "%s\n"\
		"-module(test)." \
		"-export([test/0])." \
		"test() -> old." > $(APP)/src/test.erl

	$i "Build and create initial release via relx"
	$t $(MAKE) -C $(APP) RELX_OPTS="release tar"

	$i "Make changes to test module"
	$t sed -i s/"test() -> old."/"test() -> new."/ $(APP)/src/test.erl

ifeq ($(LEGACY),1)
	$i "Change vsn tuple in .app.src"
	$t sed -i s/"{vsn, \"1\"}"/"{vsn, \"2\"}"/ $(APP)/src/$(APP).app.src
else
	$i "Change PROJECT_VERSION in Makefile"
	$t sed -i s/"PROJECT_VERSION = 1"/"PROJECT_VERSION = 2"/ $(APP)/Makefile
endif

	$i "Write .appup"
	$t printf "%s\n" \
		"{\"2\","\
		"  [{\"1\", [{load_module, test}]}],"\
		"  [{\"1\", [{load_module, test}]}]"\
		"}." > $(APP)/ebin/$(APP).appup

	$i "Bump release version"
	$t sed -i s/"1"/"2"/ $(APP)/relx.config

	$i "Build updated app, release and relup"
	$t $(MAKE) -C $(APP) RELX_OPTS="release relup tar"

	$i "Test that both releases are available"
	$t test -f $(APP)/_rel/$(APP)_release/$(APP)_release-1.tar.gz
	$t test -f $(APP)/_rel/$(APP)_release/$(APP)_release-2.tar.gz

	$i "Unpack release '1' to tmp dir"
	$t mkdir $(APP)/tmp
	$t tar -xzf $(APP)/_rel/$(APP)_release/$(APP)_release-1.tar.gz -C $(APP)/tmp

	$i "Start release '1' and query test:test()"
ifeq ($(PLATFORM),msys2)
	$t $(APP)/tmp/bin/$(APP)_release.cmd install $v
	$t $(APP)/tmp/bin/$(APP)_release.cmd start $v
	$i "Sleeping for 1 second"
	$t sleep 1
	$t test `$(APP)/tmp/bin/$(APP)_release.cmd rpcterms test test` = old
else
	$t $(APP)/tmp/bin/$(APP)_release start $v
	$i "Sleeping for 1 second"
	$t sleep 1
	$t test `$(APP)/tmp/bin/$(APP)_release rpcterms test test` = old
endif

	$i "Move relup to release directory and upgrade"
	$t mkdir $(APP)/tmp/releases/2
	$t mv $(APP)/_rel/$(APP)_release/$(APP)_release-2.tar.gz $(APP)/tmp/releases/2/$(APP)_release.tar.gz

ifeq ($(PLATFORM),msys2)
	$i "Upgrade running release"
	$t $(APP)/tmp/bin/$(APP)_release.cmd upgrade "2/$(APP)_release" $v
	$i "Sleeping for 1 second"
	$t sleep 1
	$i "Query updated test:test()"
	$t test `$(APP)/tmp/bin/$(APP)_release.cmd rpcterms test test` = new

	$i "Stop and uninstall release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd stop $v
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd uninstall $v
else
	$i "Upgrade running release"
	$t $(APP)/tmp/bin/$(APP)_release upgrade "2/$(APP)_release" $v
	$i "Sleeping for 1 second"
	$t sleep 1
	$i "Query updated test:test()"
	$t test `$(APP)/tmp/bin/$(APP)_release rpcterms test test` = new
	$i "Stop release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release stop $v
endif
