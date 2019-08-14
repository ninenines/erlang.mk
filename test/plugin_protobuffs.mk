# Protocol buffers plugin.

PROTOBUFFS_TARGETS = $(call list_targets,protobuffs)

.PHONY: protobuffs $(PROTOBUFFS_TARGETS)

protobuffs: $(PROTOBUFFS_TARGETS)

PROTOBUFFS_URL = https://raw.githubusercontent.com/basho/erlang_protobuffs/master/test/erlang_protobuffs_SUITE_data

protobuffs-compile: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add protobuffs to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = protobuffs\n"}' $(APP)/Makefile

	$i "Download two proto files"
	$t mkdir $(APP)/src/proto/
	$t curl -s -o $(APP)/src/proto/empty.proto $(PROTOBUFFS_URL)/proto/empty.proto
	$t curl -s -o $(APP)/src/proto/simple.proto $(PROTOBUFFS_URL)/proto/simple.proto

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that an Erlang module was generated and compiled"
	$t test -f $(APP)/src/empty_pb.erl
	$t test -f $(APP)/src/simple_pb.erl
	$t test -f $(APP)/include/empty_pb.hrl
	$t test -f $(APP)/include/simple_pb.hrl
	$t test -f $(APP)/ebin/empty_pb.beam
	$t test -f $(APP)/ebin/simple_pb.beam

	$i "Check that the generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [empty_pb, simple_pb]} = application:get_key($(APP), modules), \
		halt()"

protobuffs-compile-imports: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add protobuffs to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = protobuffs\n"}' $(APP)/Makefile

	$i "Download two proto files with an import"
	$t mkdir $(APP)/src/proto/
	$t curl -s -o $(APP)/src/proto/exports.proto $(PROTOBUFFS_URL)/proto/exports.proto
	$t curl -s -o $(APP)/src/proto/imports.proto $(PROTOBUFFS_URL)/proto/imports.proto

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that an Erlang module was generated and compiled"
	$t test -f $(APP)/src/exports_pb.erl
	$t test -f $(APP)/src/imports_pb.erl
	$t test -f $(APP)/include/exports_pb.hrl
	$t test -f $(APP)/include/imports_pb.hrl
	$t test -f $(APP)/ebin/exports_pb.beam
	$t test -f $(APP)/ebin/imports_pb.beam

	$i "Check that the generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [exports_pb, imports_pb]} = application:get_key($(APP), modules), \
		halt()"

protobuffs-compile-with-gpb: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add gpb to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = gpb\n"}' $(APP)/Makefile

	$i "Download two proto files"
	$t mkdir $(APP)/src/proto/
	$t curl -s -o $(APP)/src/proto/empty.proto $(PROTOBUFFS_URL)/proto/empty.proto
	$t curl -s -o $(APP)/src/proto/simple.proto $(PROTOBUFFS_URL)/proto/simple.proto

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that an Erlang module was generated and compiled"
	$t test -f $(APP)/src/empty_pb.erl
	$t test -f $(APP)/src/simple_pb.erl
	$t test -f $(APP)/include/empty_pb.hrl
	$t test -f $(APP)/include/simple_pb.hrl
	$t test -f $(APP)/ebin/empty_pb.beam
	$t test -f $(APP)/ebin/simple_pb.beam

	$i "Check that the generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [empty_pb, simple_pb]} = application:get_key($(APP), modules), \
		halt()"

protobuffs-compile-imports-with-gpb: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add gpb to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = gpb\n"}' $(APP)/Makefile

	$i "Download two proto files with an import"
	$t mkdir $(APP)/src/proto/
	$t curl -s -o $(APP)/src/proto/exports.proto $(PROTOBUFFS_URL)/proto/exports.proto
	$t curl -s -o $(APP)/src/proto/imports.proto $(PROTOBUFFS_URL)/proto/imports.proto

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that an Erlang module was generated and compiled"
	$t test -f $(APP)/src/exports_pb.erl
	$t test -f $(APP)/src/imports_pb.erl
	$t test -f $(APP)/include/exports_pb.hrl
	$t test -f $(APP)/include/imports_pb.hrl
	$t test -f $(APP)/ebin/exports_pb.beam
	$t test -f $(APP)/ebin/imports_pb.beam

	$i "Check that the generated modules are included in .app file"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:load($(APP)), \
		{ok, [exports_pb, imports_pb]} = application:get_key($(APP), modules), \
		halt()"

protobuffs-makefile-change: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add protobuffs to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "BUILD_DEPS = protobuffs\n"}' $(APP)/Makefile

	$i "Download two proto files"
	$t mkdir $(APP)/src/proto/
	$t curl -s -o $(APP)/src/proto/empty.proto $(PROTOBUFFS_URL)/proto/empty.proto
	$t curl -s -o $(APP)/src/proto/simple.proto $(PROTOBUFFS_URL)/proto/simple.proto

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Touch the Makefile; check that all files get rebuilt"
	$t printf "%s\n" \
		$(APP)/ebin/empty_pb.beam \
		$(APP)/ebin/simple_pb.beam \
		$(APP)/ebin/$(APP).app \
		$(APP)/include/empty_pb.hrl \
		$(APP)/include/simple_pb.hrl \
		$(APP)/src/empty_pb.erl \
		$(APP)/src/proto/simple.proto \
		$(APP)/src/proto/empty.proto \
		$(APP)/src/simple_pb.erl \
		$(APP)/$(APP).d | sort > $(APP)/EXPECT
	$t $(SLEEP)
	$t touch $(APP)/Makefile
	$t $(SLEEP)
	$t $(MAKE) -C $(APP) $v
	$t find $(APP) -type f -newer $(APP)/Makefile -not -path "$(APP)/.erlang.mk/*" | sort | diff $(APP)/EXPECT -
	$t rm $(APP)/EXPECT
