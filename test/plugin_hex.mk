# Hex plugin.
#
# In order to run these tests, a local Hex.pm instance must
# be started and available on port 4000, and the HEX=1 variable
# must be set.

hex_TARGETS = $(call list_targets,hex)

.PHONY: hex $(hex_TARGETS)

hex: $(hex_TARGETS)

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

# @todo Fix this.
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

# @todo Fix this.
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

hex-tarball-create: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a release tarball"
	$t $(MAKE) -C $(APP) hex-tarball-create $v

	$i "Confirm the tarball contents can be extracted"
	$t cd $(APP)/.erlang.mk/ && tar xf $(APP).tar

	$i "Confirm the tarball contains a CHECKSUM file"
	$t test -f $(APP)/.erlang.mk/CHECKSUM

	$i "Confirm the tarball contains a VERSION file containing '3'"
	$t cat $(APP)/.erlang.mk/VERSION | grep -q ^3$$

	$i "Confirm the tarball contains a valid metadata.config file"
	$t $(ERL) -eval " \
		{ok, _} = file:consult(\"$(APP)/.erlang.mk/metadata.config\"), \
		halt(0)"

	$i "Confirm the tarball contains a contents.tar.gz file that can be extracted"
	$t cd $(APP)/.erlang.mk/ && tar xf contents.tar.gz

	$i "Confirm contents.tar.gz contains the expected files"
	$t printf "%s\n" \
		erlang.mk \
		Makefile \
		ebin/$(APP).app \
		src/$(APP)_app.erl \
		src/$(APP)_sup.erl | sort > $(APP)/.erlang.mk/EXPECT
	$t cd $(APP)/.erlang.mk/ && tar tf contents.tar.gz | sort | diff EXPECT -

hex-tarball-create-with-deps: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add Cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib_commit = 2.13.0\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowlib to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowlib,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Create a release tarball"
	$t $(MAKE) -C $(APP) hex-tarball-create $v

	$i "Confirm the tarball contents can be extracted"
	$t cd $(APP)/.erlang.mk/ && tar xf $(APP).tar

	$i "Confirm the tarball contains a metadata.config file that lists Cowlib as requirement"
	$t $(ERL) -eval " \
		{ok, Metadata} = file:consult(\"$(APP)/.erlang.mk/metadata.config\"), \
		{_, [{<<\"cowlib\">>, Cowlib}]} = lists:keyfind(<<\"requirements\">>, 1, Metadata), \
		{_, <<\"cowlib\">>} = lists:keyfind(<<\"app\">>, 1, Cowlib), \
		{_, false} = lists:keyfind(<<\"optional\">>, 1, Cowlib), \
		{_, <<\"2.13.0\">>} = lists:keyfind(<<\"requirement\">>, 1, Cowlib), \
		halt(0)"

hex-tarball-create-with-req: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add Cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib_commit = 2.13.0\nhex_req_cowlib = ~> 2.13\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Cowlib to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tcowlib,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Create a release tarball"
	$t $(MAKE) -C $(APP) hex-tarball-create $v

	$i "Confirm the tarball contents can be extracted"
	$t cd $(APP)/.erlang.mk/ && tar xf $(APP).tar

	$i "Confirm the tarball contains a metadata.config file that lists Cowlib as requirement"
	$t $(ERL) -eval " \
		{ok, Metadata} = file:consult(\"$(APP)/.erlang.mk/metadata.config\"), \
		{_, [{<<\"cowlib\">>, Cowlib}]} = lists:keyfind(<<\"requirements\">>, 1, Metadata), \
		{_, <<\"cowlib\">>} = lists:keyfind(<<\"app\">>, 1, Cowlib), \
		{_, false} = lists:keyfind(<<\"optional\">>, 1, Cowlib), \
		{_, <<\"~> 2.13\">>} = lists:keyfind(<<\"requirement\">>, 1, Cowlib), \
		halt(0)"

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

# @todo There's a weird ci.erlang.mk related bug with CACHE_DEPS. Fix it.
ifndef CACHE_DEPS
hex-release-publish-with-deps: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Add Cowlib to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib_commit = 2.13.0\n"}' $(APP)/Makefile

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
	$t cp ../erlang.mk $(APP)/deps/cowlib
	$t $(MAKE) -C $(APP)/deps/cowlib hex-release-publish DEPS_DIR=$(APP)/deps ERLANG_MK_TMP=$(APP)/.erlang.mk HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Publish the release"
	$t $(MAKE) -C $(APP) hex-release-publish HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Check that the release exists and includes Cowlib as requirement"
	$t curl -sf http://localhost:4000/api/packages/$(APP)/releases/0.1.0 | grep -q cowlib
endif

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

hex-docs-tarball-create: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a doc/overview.edoc file"
	$t mkdir $(APP)/doc
	$t printf "%s\n" \
		"@author R. J. Hacker <rjh@acme.com>" \
		"@copyright 2007 R. J. Hacker" \
		"@version 1.0.0" \
		"@title Welcome to the 'frob' application!" \
		"@doc 'frob' is a highly advanced frobnicator with low latency," > $(APP)/doc/overview.edoc

	$i "Create a docs tarball"
	$t $(MAKE) -C $(APP) hex-docs-tarball-create $v

	$i "Confirm the tarball contents can be extracted"
	$t cd $(APP)/.erlang.mk/ && tar xf $(APP)-docs.tar.gz

	$i "Confirm the tarball contains the expected files"
	$t printf "%s\n" \
		edoc-info \
		erlang.png \
		index.html \
		modules-frame.html \
		overview-summary.html \
		overview.edoc \
		stylesheet.css \
		$(APP)_app.html \
		$(APP)_sup.html | sort > $(APP)/.erlang.mk/EXPECT
	$t cd $(APP)/.erlang.mk/ && tar tf $(APP)-docs.tar.gz | sort | diff EXPECT -

hex-docs-publish: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a doc/overview.edoc file"
	$t mkdir $(APP)/doc
	$t printf "%s\n" \
		"@author R. J. Hacker <rjh@acme.com>" \
		"@copyright 2007 R. J. Hacker" \
		"@version 1.0.0" \
		"@title Welcome to the 'frob' application!" \
		"@doc 'frob' is a highly advanced frobnicator with low latency," > $(APP)/doc/overview.edoc

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

	$i "Publish the documentation for the release"
	$t $(MAKE) -C $(APP) hex-docs-publish HEX_SECRET=`cat $(APP)/hex.key` $v

# @todo hex-docs-publish when there are no docs

hex-docs-delete: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Generate a doc/overview.edoc file"
	$t mkdir $(APP)/doc
	$t printf "%s\n" \
		"@author R. J. Hacker <rjh@acme.com>" \
		"@copyright 2007 R. J. Hacker" \
		"@version 1.0.0" \
		"@title Welcome to the 'frob' application!" \
		"@doc 'frob' is a highly advanced frobnicator with low latency," > $(APP)/doc/overview.edoc

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

	$i "Publish the documentation for the release"
	$t $(MAKE) -C $(APP) hex-docs-publish HEX_SECRET=`cat $(APP)/hex.key` $v

	$i "Delete the documentation for the release"
	$t $(MAKE) -C $(APP) hex-docs-delete HEX_SECRET=`cat $(APP)/hex.key` $v
endif
