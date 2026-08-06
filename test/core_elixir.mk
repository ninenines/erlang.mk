# Core: Miscellaneous.
#
# The miscellaneous tests use the prefix "core-", not "core-misc-".

CORE_ELIXIR_TARGETS = $(call list_targets,core-elixir)

.PHONY: core-elixir $(CORE_ELIXIR_TARGETS)

core-elixir: $(CORE_ELIXIR_TARGETS)

core-elixir-compile-from-lib: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create Elixir source file hello.ex"
	$t mkdir $(APP)/lib
	$t printf "%s\n" \
		"defmodule HelloWorld do" \
		"  def hello do" \
		'	IO.puts("Hello, world!")' \
		"  end" \
		"end" > $(APP)/lib/hello.ex

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/Elixir.HelloWorld.beam

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -pa $(APP)/deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
		ok = application:start($(APP)), \
		{ok, Mods = ['Elixir.HelloWorld']} \
			= application:get_key($(APP), modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"

core-elixir-compile-from-src: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create Elixir source file hello.ex"
	$t printf "%s\n" \
		"defmodule HelloWorld do" \
		"  def hello do" \
		'	IO.puts("Hello, world!")' \
		"  end" \
		"end" > $(APP)/src/hello.ex

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/Elixir.HelloWorld.beam

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -pa $(APP)/deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
		ok = application:start($(APP)), \
		{ok, Mods = ['Elixir.HelloWorld']} \
			= application:get_key($(APP), modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"

core-elixir-disable: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create Elixir source file hello.ex"
	$t printf "%s\n" \
		"defmodule HelloWorld do" \
		"  def hello do" \
		'	IO.puts("Hello, world!")' \
		"  end" \
		"end" > $(APP)/src/hello.ex

	$i "Disable Elixir in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "ELIXIR = disable\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the Elixir file wasn't compiled"
	$t test -f $(APP)/ebin/$(APP).app
	$t test ! -e $(APP)/ebin/Elixir.HelloWorld.beam

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -pa $(APP)/deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
		ok = application:start($(APP)), \
		{ok, Mods = []} \
			= application:get_key($(APP), modules), \
		halt()"

core-elixir-disable-autopatch-fail: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Jason to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = jason\ndep_jason = git https://github.com/michalmuskala/jason.git master\n"}' $(APP)/Makefile

	$i "Disable Elixir in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "ELIXIR = disable\n"}' $(APP)/Makefile

	$i "Building the application should fail"
	$t ! $(MAKE) -C $(APP) $v

core-elixir-disable-autopatch-erlang-mk: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Jose to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = jose\ndep_jose = git https://github.com/potatosalad/erlang-jose main\n"}' $(APP)/Makefile

	$i "Disable Elixir in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "ELIXIR = disable\n"}' $(APP)/Makefile

	$i "Building the application should work as Jose is Erlang.mk-compatible"
	$t $(MAKE) -C $(APP) $v

core-elixir-disable-autopatch-make: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Reloader to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = reloader\ndep_reloader = git https://github.com/2600hz/erlang-reloader de1e6c74204b61ccf3b3652f05c6a7dec9e8257d\n"}' $(APP)/Makefile

	$i "Disable Elixir in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "ELIXIR = disable\n"}' $(APP)/Makefile

	$i "Fetch dependencies to patch 'reloader'"
	$t $(MAKE) -C $(APP) fetch-deps $v

# Patch `reloader` Makefile to be compatible with BSD sed. Its Makefile called
# sed(1) in a way that was only compatible with GNU sed. As a consequence, the
# build failed with BSD sed.
	$i "Patch sed(1) use in Makefile"
	$t test -f $(APP)/deps/reloader/Makefile
	$t perl -pi.bak -e 's/\@sed/\@sed -E/;' -e 'if (/sed/) { s/{/\\{/g; s/}/\\}/g; s/\\s\*//; }' $(APP)/deps/reloader/Makefile

	$i "Building the application should work as Reloader contains a proper Makefile"
	$t $(MAKE) -C $(APP) $v

	$i "Confirm Reloader was built"
	$t test -f $(APP)/.erlang.mk/dep_built/reloader
	$t test -f $(APP)/deps/reloader/ebin/reloader.app
	$t test -f $(APP)/deps/reloader/ebin/reloader.beam

core-elixir-disable-autopatch-rebar3: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add OpenTelemetry_API to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = opentelemetry_api\ndep_opentelemetry_api = hex 1.3.0\n"}' $(APP)/Makefile

	$i "Disable Elixir in the Makefile"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "ELIXIR = disable\n"}' $(APP)/Makefile

	$i "Building the application should work as OpenTelemetry_API is Rebar3-compatible"
	$t $(MAKE) -C $(APP) $v

core-elixir-disable-by-default-autopatch-rebar3: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add OpenTelemetry_API to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = opentelemetry_api\ndep_opentelemetry_api = hex 1.3.0\n"}' $(APP)/Makefile

	$i "Building the application should work as OpenTelemetry_API is Rebar3-compatible"
	$t $(MAKE) -C $(APP) $v

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
	$t test -f $(APP)/.erlang.mk/dep_built/elixir
	$t test -f $(APP)/.erlang.mk/dep_built/lager
	$t test -f $(APP)/.erlang.mk/dep_built/jason
	$t test -f $(APP)/.erlang.mk/dep_built/phoenix

	$i "Check that the application was compiled correctly"
	$t cd $(APP); $(ERL) -pa ebin/ -pa deps/*/ebin -pa deps/elixir/lib/*/ebin -eval " \
		{ok, Apps} = application:ensure_all_started('$(APP)'), \
		true = lists:member(elixir, Apps), \
		true = lists:member(lager, Apps), \
		true = lists:member(jason, Apps), \
		true = lists:member(phoenix, Apps), \
		halt()"

	$i "Check that the Jason application depends on Elixir builtins"
	$t cd $(APP); $(ERL) -pa ebin/ -pa deps/*/ebin -pa deps/elixir/lib/*/ebin -eval " \
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
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager jason phoenix\ndep_lager = git https://github.com/erlang-lager/lager master\ndep_jason = git https://github.com/michalmuskala/jason.git master\ndep_phoenix = hex 1.7.2\nELIXIR = system\n"}' $(APP)/Makefile

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
	$t test -f $(APP)/.erlang.mk/dep_built/lager
	$t test -f $(APP)/.erlang.mk/dep_built/jason
	$t test -f $(APP)/.erlang.mk/dep_built/phoenix

	$i "Check that the application was compiled correctly"
	$t cd $(APP); $(ERL) -pa ebin/ -pa deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
		{ok, Apps} = application:ensure_all_started('$(APP)'), \
		true = lists:member(lager, Apps), \
		true = lists:member(jason, Apps), \
		true = lists:member(phoenix, Apps), \
		halt()"

	$i "Check that the Jason application depends on Elixir builtins"
	$t cd $(APP); $(ERL) -pa ebin/ -pa deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
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
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = libsalty2\ndep_libsalty2 = git https://github.com/Ianleeclark/libsalty2.git b11e544\nELIXIR = system\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add Libsalty2 to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tlibsalty2,\n"}' $(APP)/src/$(APP).app.src
endif

# Specify CFLAGS when building the Elixir NIF. On FreeBSD, libsodium's
# `sodium.h` header is installed in `/usr/local/local`. The Makefile already
# adds `/usr/local/include/sodium` to the compiler's `-I` search path, but it
# doesn't cover the FreeBSD case.
	$i "Build the application"
	$t $(MAKE) -C $(APP) $v CFLAGS=-I/usr/local/include

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -pa $(APP)/deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
		{ok, Apps} = application:ensure_all_started('$(APP)'), \
		true = lists:member(libsalty2, Apps), \
		halt()"

# Exqlite is a Mix package that builds its NIF via the `:elixir_make`
# compiler (`compilers: [:elixir_make]` in its mix.exs), rather than
# through a rebar/erlang.mk-style c_src/Makefile like Libsalty2 above.
# dep_autopatch_mix's elixir_make branch generates the Makefile that
# builds it, and has historically had bugs distinct from the plain-NIF
# path core-elixir-nif exercises above:
#   - the generated `app::` recipe concatenated `-f elixir_make.mk`
#     directly onto the make targets with no separating space,
#     producing a nonexistent `elixir_make.mkall` target;
#   - MIX_APP_PATH/ERTS_INCLUDE_DIR/ERL_EI_INCLUDE_DIR/ERL_EI_LIBDIR,
#     which Mix normally injects automatically, were never set, so the
#     dependency's own build recipe couldn't find its headers or its
#     output directory;
#   - erlang.mk's own generic c_src auto-build additionally runs
#     unconditionally for any dependency directory containing a c_src/
#     folder, regardless of whether that dependency already has its own
#     elixir_make-driven recipe, and recompiles the same sources a
#     second time without the flags the first build used.
core-elixir-nif-elixir-make: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Exqlite to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = exqlite\ndep_exqlite = hex 0.39.0\nELIXIR = system\n"}' $(APP)/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that the NIF shared library was built"
	$t test -f $(APP)/deps/exqlite/priv/sqlite3_nif.so

	$i "Check that the NIF actually loads and can open a database"
	$t $(ERL) -pa $(APP)/deps/exqlite/ebin -pa $(APP)/deps/*/ebin -pa $(dir $(shell elixir -e 'IO.puts(:code.lib_dir(:elixir))'))/*/ebin -eval " \
		{ok, Conn} = 'Elixir.Exqlite.Sqlite3':open(<<\":memory:\">>), \
		ok = 'Elixir.Exqlite.Sqlite3':close(Conn), \
		halt()"

core-elixir-rel: init

	$i "Bootstrap a new release named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib bootstrap-rel $v

	$i "Add Lager, Jason, Phoenix to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = lager jason phoenix\ndep_lager = git https://github.com/erlang-lager/lager master\ndep_jason = git https://github.com/michalmuskala/jason.git master\ndep_phoenix = hex 1.7.2\nELIXIR = system\n"}' $(APP)/Makefile

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
