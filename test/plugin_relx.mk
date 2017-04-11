# Relx plugin.
#
# Sleeps when interacting with relx script are necessary after start and upgrade
# as both of those interactions are not synchronized.

RELX_CASES = rel relup script semver start-stop tar
RELX_TARGETS = $(addprefix relx-,$(RELX_CASES))

.PHONY: relx $(RELX_TARGETS)

relx: $(RELX_TARGETS)

relx-rel: build clean

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that relx was downloaded"
	$t test -f $(APP)/.erlang.mk/relx

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

relx-script: build clean

	$i "Bootstrap a new release name $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Create relx script that sets the release version"

	$t echo "{release, {Name, _Ver}, Apps} = lists:keyfind(release, 1, CONFIG)," > $(APP)/relx.config.script
	$t echo "lists:keystore(release, 1, CONFIG, {release, {Name, \"2\"}, Apps})." >> $(APP)/relx.config.script

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the release version is correct"
	$t test -d $(APP)/_rel/$(APP)_release/releases/2

relx-semver: build clean

	$i "Bootstrap a new release name $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Set relx version to 'semver'"
	$t sed -i.bak s/"{$(APP)_release, \"1\"}"/"{$(APP)_release, semver}"/ $(APP)/relx.config

	$i "Init git repo"
	$t touch $(APP)/repo_file
	$t touch $(APP)/git.log
	$t git -C $(APP)/ init >> $(APP)/git.log
	$t git -C $(APP)/ add repo_file >> $(APP)/git.log 
	$t git -C $(APP)/ commit --no-verify --message="Initial commit" >> $(APP)/git.log
	$t git -C $(APP)/ tag --force v2.0.0 >> $(APP)/git.log

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the release version is correct"
	$t test -d $(APP)/_rel/$(APP)_release/releases/2.0.0
	
	$i "Create new commit to dirty the version"
	$t echo "-" >> $(APP)/repo_file
	$t git -C $(APP) add repo_file >> $(APP)/git.log
	$t git -C $(APP) commit --no-verify --message="Update commit" >> $(APP)/git.log

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the new release is correct"
	$t test -d $(APP)/_rel/$(APP)_release/releases/2.0.0+build.1.ref*

	$i "Create new version tag"
	$t git -C $(APP)/ tag --force v2.1.0 >> $(APP)/git.log

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the new release is correct"
	$t test -d $(APP)/_rel/$(APP)_release/releases/2.1.0

relx-relup: build clean

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

	$i "Start initial release and confirm it runs the old code"
ifeq ($(PLATFORM),msys2)
	$t $(APP)/tmp/bin/$(APP)_release.cmd install $v
	$t $(APP)/tmp/bin/$(APP)_release.cmd start $v
	$t sleep 1
	$t test `$(APP)/tmp/bin/$(APP)_release.cmd rpcterms test test` = old
else
	$t $(APP)/tmp/bin/$(APP)_release start $v
	$t sleep 1
	$t test `$(APP)/tmp/bin/$(APP)_release rpcterms test test` = old
endif

	$i "Move the relup tarball to the release directory"
	$t mkdir $(APP)/tmp/releases/2
	$t mv $(APP)/_rel/$(APP)_release/$(APP)_release-2.tar.gz $(APP)/tmp/releases/2/$(APP)_release.tar.gz

	$i "Upgrade the release and confirm it runs the new code"
ifeq ($(PLATFORM),msys2)
	$t $(APP)/tmp/bin/$(APP)_release.cmd upgrade "2/$(APP)_release" $v
	$t sleep 1
	$t test `$(APP)/tmp/bin/$(APP)_release.cmd rpcterms test test` = new

	$i "Stop the release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd stop $v
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd uninstall $v
else
	$i "Upgrade running release"
	$t $(APP)/tmp/bin/$(APP)_release upgrade "2/$(APP)_release" $v
	$t sleep 1
	$t test `$(APP)/tmp/bin/$(APP)_release rpcterms test test` = new

	$i "Stop the release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release stop $v
endif

relx-start-stop: build clean

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Build the release"
	$t $(MAKE) -C $(APP) $v

ifeq ($(PLATFORM),msys2)
	$i "Install and start the release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd install $v
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd start $v
	$t sleep 1

	$i "Ping the release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd ping $v

	$i "Stop and uninstall the release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd stop $v
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd uninstall $v

	$i "Check that further pings get no replies"
	$t ! $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd ping $v
else
	$i "Start the release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release start $v
	$t sleep 1

	$i "Ping the release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release ping $v

	$i "Stop the release"
	$t $(APP)/_rel/$(APP)_release/bin/$(APP)_release stop $v

	$i "Check that further pings get no replies"
	$t ! $(APP)/_rel/$(APP)_release/bin/$(APP)_release ping $v
endif

relx-tar: build clean

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Build the release as a tarball"
	$t $(MAKE) -C $(APP) $v

	$i "Check that tarball exists"
	$t test -f $(APP)/_rel/$(APP)_release/$(APP)_release-1.tar.gz
