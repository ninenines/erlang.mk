# Bootstrap plugin.

bootstrap_TARGETS = $(call list_targets,bootstrap)

.PHONY: bootstrap $(bootstrap_TARGETS)

bootstrap: $(bootstrap_TARGETS)

bootstrap-app: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/Makefile
ifdef LEGACY
	$t test -f $(APP)/src/$(APP).app.src
endif
	$t test -f $(APP)/src/$(APP)_app.erl
	$t test -f $(APP)/src/$(APP)_sup.erl

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP)_app.beam
	$t test -f $(APP)/ebin/$(APP)_sup.beam

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, [$(APP)_app, $(APP)_sup]} = application:get_key($(APP), modules), \
		{module, $(APP)_app} = code:load_file($(APP)_app), \
		{module, $(APP)_sup} = code:load_file($(APP)_sup), \
		halt()"

bootstrap-invalid-app-name-dash: init

	$i "Try to bootstrap a new OTP application named test_$@"
	$t mkdir test_$@/
	$t cp ../erlang.mk test_$@/
	$t ! $(MAKE) -C test_$@ -f erlang.mk bootstrap $v

bootstrap-invalid-app-name-uppercase: init

	$i "Try to bootstrap a new OTP application named $(APP)_HELLO"
	$t mkdir $(APP)_HELLO/
	$t cp ../erlang.mk $(APP)_HELLO/
	$t ! $(MAKE) -C $(APP)_HELLO -f erlang.mk bootstrap $v

bootstrap-invalid-lib-name-dash: init

	$i "Try to bootstrap a new OTP library named test_$@"
	$t mkdir test_$@/
	$t cp ../erlang.mk test_$@/
	$t ! $(MAKE) -C test_$@ -f erlang.mk bootstrap-lib $v

bootstrap-invalid-lib-name-uppercase: init

	$i "Try to bootstrap a new OTP library named $(APP)_HELLO"
	$t mkdir $(APP)_HELLO/
	$t cp ../erlang.mk $(APP)_HELLO/
	$t ! $(MAKE) -C $(APP)_HELLO -f erlang.mk bootstrap-lib $v

bootstrap-invalid-new-app-name-dash: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Try to create a new application my-app"
	$t ! $(MAKE) -C $(APP) new-app in=my-app $v

bootstrap-invalid-new-app-name-uppercase: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Try to create a new application My_app"
	$t ! $(MAKE) -C $(APP) new-app in=My_app $v

bootstrap-invalid-new-lib-name-dash: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Try to create a new library my-lib"
	$t ! $(MAKE) -C $(APP) new-lib in=my-lib $v

bootstrap-invalid-new-lib-name-uppercase: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Try to create a new library My_lib"
	$t ! $(MAKE) -C $(APP) new-lib in=My_lib $v

bootstrap-lib: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/Makefile
ifdef LEGACY
	$t test -f $(APP)/src/$(APP).app.src
endif

	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, []} = application:get_key($(APP), modules), \
		halt()"

bootstrap-new-app-sp: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap SP=2 $v

	$i "Create a new application my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app $v

	$i "Check that SP is included in the new Makefile"
	$t grep -q "SP = 2" $(APP)/apps/my_app/Makefile

	$i "Check that bootstrapped files have no tabs"
ifdef LEGACY
	$t test -z "`awk -F "\t" 'NF > 1' $(APP)/apps/my_app/src/my_app.app.src`"
endif
	$t test -z "`awk -F "\t" 'NF > 1' $(APP)/apps/my_app/src/my_app_app.erl`"
	$t test -z "`awk -F "\t" 'NF > 1' $(APP)/apps/my_app/src/my_app_sup.erl`"

# Everything looks OK, but let's compile the application to make sure.
	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP)_app.beam
	$t test -f $(APP)/ebin/$(APP)_sup.beam
	$t test -f $(APP)/apps/my_app/ebin/my_app.app
	$t test -f $(APP)/apps/my_app/ebin/my_app_app.beam
	$t test -f $(APP)/apps/my_app/ebin/my_app_sup.beam

bootstrap-new-app-sp-override: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap SP=2 $v

	$i "Create a new application my_app"
	$t $(MAKE) -C $(APP) new-app in=my_app SP=8 $v

	$i "Check that the SP we provided is included in the new Makefile"
	$t grep -q "SP = 8" $(APP)/apps/my_app/Makefile

	$i "Check that bootstrapped files have no tabs"
ifdef LEGACY
	$t test -z "`awk -F "\t" 'NF > 1' $(APP)/apps/my_app/src/my_app.app.src`"
endif
	$t test -z "`awk -F "\t" 'NF > 1' $(APP)/apps/my_app/src/my_app_app.erl`"
	$t test -z "`awk -F "\t" 'NF > 1' $(APP)/apps/my_app/src/my_app_sup.erl`"

# Everything looks OK, but let's compile the application to make sure.
	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP)_app.beam
	$t test -f $(APP)/ebin/$(APP)_sup.beam
	$t test -f $(APP)/apps/my_app/ebin/my_app.app
	$t test -f $(APP)/apps/my_app/ebin/my_app_app.beam
	$t test -f $(APP)/apps/my_app/ebin/my_app_sup.beam

bootstrap-rel: init

	$i "Bootstrap a new release-enabled OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap bootstrap-rel $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/Makefile
	$t test -f $(APP)/relx.config
	$t test -f $(APP)/config/sys.config
	$t test -f $(APP)/config/vm.args
ifdef LEGACY
	$t test -f $(APP)/src/$(APP).app.src
endif
	$t test -f $(APP)/src/$(APP)_app.erl
	$t test -f $(APP)/src/$(APP)_sup.erl

	$i "Build the application and the release"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP)_app.beam
	$t test -f $(APP)/ebin/$(APP)_sup.beam

	$i "Check that the release was generated"
ifeq ($(PLATFORM),msys2)
	$t test -f $(APP)/_rel/$(APP)_release/bin/$(APP)_release.cmd
else
	$t test -f $(APP)/_rel/$(APP)_release/bin/$(APP)_release
endif

bootstrap-sp: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap SP=2 $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/Makefile
ifdef LEGACY
	$t test -f $(APP)/src/$(APP).app.src
endif
	$t test -f $(APP)/src/$(APP)_app.erl
	$t test -f $(APP)/src/$(APP)_sup.erl

	$i "Check that bootstrapped files have no tabs"
ifdef LEGACY
	$t test -z "`awk -F "\t" 'NF > 1' $(APP)/src/$(APP).app.src`"
endif
	$t test -z "`awk -F "\t" 'NF > 1' $(APP)/src/$(APP)_app.erl`"
	$t test -z "`awk -F "\t" 'NF > 1' $(APP)/src/$(APP)_sup.erl`"

# Everything looks OK, but let's compile the application to make sure.
	$i "Build the application"
	$t $(MAKE) -C $(APP) $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/$(APP)_app.beam
	$t test -f $(APP)/ebin/$(APP)_sup.beam

	$i "Check that the application was compiled correctly"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, [$(APP)_app, $(APP)_sup]} = application:get_key($(APP), modules), \
		{module, $(APP)_app} = code:load_file($(APP)_app), \
		{module, $(APP)_sup} = code:load_file($(APP)_sup), \
		halt()"

bootstrap-tab: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Check that all bootstrapped files exist"
	$t test -f $(APP)/Makefile
ifdef LEGACY
	$t test -f $(APP)/src/$(APP).app.src
endif
	$t test -f $(APP)/src/$(APP)_app.erl
	$t test -f $(APP)/src/$(APP)_sup.erl

	$i "Check that bootstrapped files have tabs"
ifdef LEGACY
	$t test "`awk -F "\t" 'NF > 1' $(APP)/src/$(APP).app.src`"
endif
	$t test "`awk -F "\t" 'NF > 1' $(APP)/src/$(APP)_app.erl`"
	$t test "`awk -F "\t" 'NF > 1' $(APP)/src/$(APP)_sup.erl`"

bootstrap-templates: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Check that we can get the list of templates"
	$t test `$(MAKE) -C $(APP) --no-print-directory list-templates V=0 | wc -l` -gt 1

	$i "Generate one of each template"
	$t $(MAKE) -C $(APP) --no-print-directory new t=gen_fsm n=my_fsm
	$t $(MAKE) -C $(APP) --no-print-directory new t=gen_statem n=my_statem
	$t $(MAKE) -C $(APP) --no-print-directory new t=gen_server n=my_server
	$t $(MAKE) -C $(APP) --no-print-directory new t=supervisor n=my_sup
	$t $(MAKE) -C $(APP) --no-print-directory new t=cowboy_http_h n=my_http_h
	$t $(MAKE) -C $(APP) --no-print-directory new t=cowboy_loop_h n=my_loop_h
	$t $(MAKE) -C $(APP) --no-print-directory new t=cowboy_rest_h n=my_rest_h
	$t $(MAKE) -C $(APP) --no-print-directory new t=cowboy_websocket_h n=my_ws_h
	$t $(MAKE) -C $(APP) --no-print-directory new t=ranch_protocol n=my_protocol
	$t $(MAKE) -C $(APP) --no-print-directory new t=module n=my_module

	$i "Confirm we can't overwrite existing files"
	$t ! $(MAKE) -C $(APP) --no-print-directory new t=gen_server n=my_server $v

# Here we disable warnings because templates contain missing behaviors.
	$i "Build the application"
	$t $(MAKE) -C $(APP) ERLC_OPTS=+debug_info $v

	$i "Check that all compiled files exist"
	$t test -f $(APP)/ebin/$(APP).app
	$t test -f $(APP)/ebin/my_fsm.beam
	$t test -f $(APP)/ebin/my_statem.beam
	$t test -f $(APP)/ebin/my_server.beam
	$t test -f $(APP)/ebin/my_sup.beam
	$t test -f $(APP)/ebin/my_http_h.beam
	$t test -f $(APP)/ebin/my_loop_h.beam
	$t test -f $(APP)/ebin/my_rest_h.beam
	$t test -f $(APP)/ebin/my_ws_h.beam
	$t test -f $(APP)/ebin/my_protocol.beam
	$t test -f $(APP)/ebin/my_module.beam

	$i "Check that all the modules can be loaded"
	$t $(ERL) -pa $(APP)/ebin/ -eval " \
		ok = application:start($(APP)), \
		{ok, Mods = [my_fsm, my_http_h, my_loop_h, my_module, my_protocol, my_rest_h, my_server, my_statem, my_sup, my_ws_h]} \
			= application:get_key($(APP), modules), \
		[{module, M} = code:load_file(M) || M <- Mods], \
		halt()"
