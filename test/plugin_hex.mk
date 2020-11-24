# Hex plugin.
#
# In order to run these tests, a local Hex.pm instance must
# be started and available on port 4000, and the HEX=1 variable
# must be set.

HEX_TARGETS = $(call list_targets,hex)

.PHONY: hex $(HEX_TARGETS)

hex: $(HEX_TARGETS)

ifeq ($(shell netcat -z localhost 4000 && echo ok),ok)
hex-user-create: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure a local Hex provider"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile

	$i "Create a Hex user"
	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" HEX_EMAIL=$(APP)@noone.nope $v

	$i "Check that the user exists"
	$t curl -sf http://localhost:4000/api/users/$(APP) >/dev/null

#hex-user-create-password-with-dollar-sign: init
#
#	$i "Bootstrap a new OTP application named $(APP)"
#	$t mkdir $(APP)/
#	$t cp ../erlang.mk $(APP)/
#	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v
#
#	$i "Configure a local Hex provider"
#	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile
#
#	$i "Create a Hex user"
#	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="123$$567" HEX_EMAIL=$(APP)@noone.nope $v
#
#	$i "Check that the user exists"
#	$t curl --user "$(APP):123$$567" -sf http://localhost:4000/api/users/$(APP) >/dev/null

#hex-user-create-password-with-backslash: init
#
#	$i "Bootstrap a new OTP application named $(APP)"
#	$t mkdir $(APP)/
#	$t cp ../erlang.mk $(APP)/
#	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v
#
#	$i "Configure a local Hex provider"
#	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile
#
#	$i "Create a Hex user"
#	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="123\\567" HEX_EMAIL=$(APP)@noone.nope $v
#
#	$i "Check that the user exists"
#	$t curl --user "$(APP):123\\567" -sf http://localhost:4000/api/users/$(APP) >/dev/null

hex-user-create-password-with-space: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure a local Hex provider"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile

	$i "Create a Hex user"
	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="123 567" HEX_EMAIL=$(APP)@noone.nope $v

	$i "Check that the user exists"
	$t curl --user "$(APP):123 567" -sf http://localhost:4000/api/users/$(APP) >/dev/null

hex-key-add: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure a local Hex provider"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile

	$i "Create a Hex user"
	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" HEX_EMAIL=$(APP)@noone.nope $v

	$i "Create a key for that user"
	$t $(MAKE) -C $(APP) hex-key-add HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" $v

	$i "Check that the key exists"
	$t curl --user $(APP):1234567 -sf http://localhost:4000/api/keys/$(shell hostname)-erlang-mk >/dev/null

# @todo hex-tarball-create
# @todo hex-tarball-create-with-deps

hex-release-publish: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure a local Hex provider"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile

	$i "Add extra Hex metadata"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_TARBALL_EXTRA_METADATA\n#{licenses => [<<\"ISC\">>]}\nendef\n"}' $(APP)/Makefile

	$i "Create a Hex user"
	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" HEX_EMAIL=$(APP)@noone.nope $v

	$i "Create a key for that user"
	$t $(MAKE) -C $(APP) hex-key-add HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" | grep ^Secret: | cut -f2 -d" " > $(APP)/hex.key

	$i "Publish the release"
	$t $(MAKE) -C $(APP) hex-release-publish HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Check that the release exists"
	$t curl -sf http://localhost:4000/api/packages/$(APP)/releases/0.1.0 >/dev/null

hex-release-publish-with-deps: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add Cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib_commit = 2.10.1\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowlib to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowlib,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Configure a local Hex provider for Cowlib"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/deps/cowlib/Makefile

	$i "Configure a local Hex provider"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile

	$i "Add extra Hex metadata"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_TARBALL_EXTRA_METADATA\n#{licenses => [<<\"ISC\">>]}\nendef\n"}' $(APP)/Makefile

	$i "Create a Hex user"
	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" HEX_EMAIL=$(APP)@noone.nope $v

	$i "Create a key for that user"
	$t $(MAKE) -C $(APP) hex-key-add HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" | grep ^Secret: | cut -f2 -d" " > $(APP)/hex.key

	$i "Publish the Cowlib release"
	$t $(MAKE) -C $(APP)/deps/cowlib hex-release-publish HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Publish the release"
	$t $(MAKE) -C $(APP) hex-release-publish HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Check that the release exists and includes Cowlib as requirement"
	$t curl -sf http://localhost:4000/api/packages/$(APP)/releases/0.1.0 | grep -q cowlib

hex-release-replace: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure a local Hex provider"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile

	$i "Add extra Hex metadata"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_TARBALL_EXTRA_METADATA\n#{licenses => [<<\"ISC\">>]}\nendef\n"}' $(APP)/Makefile

	$i "Create a Hex user"
	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" HEX_EMAIL=$(APP)@noone.nope $v

	$i "Create a key for that user"
	$t $(MAKE) -C $(APP) hex-key-add HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" | grep ^Secret: | cut -f2 -d" " > $(APP)/hex.key

	$i "Publish the release"
	$t $(MAKE) -C $(APP) hex-release-publish HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Update the project description"
	$t sed -i.bak s/"PROJECT_DESCRIPTION = New project"/"PROJECT_DESCRIPTION = Best project"/ $(APP)/Makefile

	$i "Publish the release again, replacing the existing one"
	$t $(MAKE) -C $(APP) hex-release-replace HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Check that the release was replaced"
	$t curl -sf http://localhost:4000/api/packages/$(APP) | grep -q "Best project"

hex-release-delete: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure a local Hex provider"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile

	$i "Add extra Hex metadata"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_TARBALL_EXTRA_METADATA\n#{licenses => [<<\"ISC\">>]}\nendef\n"}' $(APP)/Makefile

	$i "Create a Hex user"
	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" HEX_EMAIL=$(APP)@noone.nope $v

	$i "Create a key for that user"
	$t $(MAKE) -C $(APP) hex-key-add HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" | grep ^Secret: | cut -f2 -d" " > $(APP)/hex.key

	$i "Publish the release"
	$t $(MAKE) -C $(APP) hex-release-publish HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Delete the release"
	$t $(MAKE) -C $(APP) hex-release-delete HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Check that the release was deleted"
	$t ! curl -sf http://localhost:4000/api/packages/$(APP)/releases/0.1.0 >/dev/null

hex-release-retire: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure a local Hex provider"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile

	$i "Add extra Hex metadata"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_TARBALL_EXTRA_METADATA\n#{licenses => [<<\"ISC\">>]}\nendef\n"}' $(APP)/Makefile

	$i "Create a Hex user"
	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" HEX_EMAIL=$(APP)@noone.nope $v

	$i "Create a key for that user"
	$t $(MAKE) -C $(APP) hex-key-add HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" | grep ^Secret: | cut -f2 -d" " > $(APP)/hex.key

	$i "Publish the release"
	$t $(MAKE) -C $(APP) hex-release-publish HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Retire the release"
	$t $(MAKE) -C $(APP) hex-release-retire HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Check that the release was retired"
	$t ! curl -sf http://localhost:4000/api/packages/$(APP)/releases/0.1.0 | grep -q \"retirement\":null

hex-release-unretire: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure a local Hex provider"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_CONFIG\n#{api_url => <<\"http://localhost:4000/api\">>}\nendef\n"}' $(APP)/Makefile

	$i "Add extra Hex metadata"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "define HEX_TARBALL_EXTRA_METADATA\n#{licenses => [<<\"ISC\">>]}\nendef\n"}' $(APP)/Makefile

	$i "Create a Hex user"
	$t $(MAKE) -C $(APP) hex-user-create HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" HEX_EMAIL=$(APP)@noone.nope $v

	$i "Create a key for that user"
	$t $(MAKE) -C $(APP) hex-key-add HEX_USERNAME=$(APP) HEX_PASSWORD="1234567" | grep ^Secret: | cut -f2 -d" " > $(APP)/hex.key

	$i "Publish the release"
	$t $(MAKE) -C $(APP) hex-release-publish HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Retire the release"
	$t $(MAKE) -C $(APP) hex-release-retire HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Unretire the release"
	$t $(MAKE) -C $(APP) hex-release-unretire HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Check that the release is no longer retired"
	$t curl -sf http://localhost:4000/api/packages/$(APP)/releases/0.1.0 | grep -q \"retirement\":null
endif
