# Xref plugin.

XREF_TARGETS = $(call list_targets,xref)

.PHONY: xref $(XREF_TARGETS)

xref: $(XREF_TARGETS)

xref-check: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Run the Xref plugin"
	$t $(MAKE) -C $(APP) xref $v

	$i "Create a module with an undefined function call"
	$t printf "%s\n" \
		"-module(bad)." \
		"-export([f/0])." \
		"f() -> this_module:does_not_exist()." \
		> $(APP)/src/bad.erl

	$i "Run the Xref plugin again, expect an error"
	$t ! $(MAKE) -C $(APP) xref $v

xref-check-custom: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Run the Xref plugin with undefined_functions"
	$t $(MAKE) -C $(APP) xref XREF_CHECKS=undefined_functions $v

	$i "Create a module with an unused export"
	$t printf "%s\n" \
		"-module(bad1)." \
		"-export([f/0])." \
		"f() -> whereis(user) ! bad_message." \
		> $(APP)/src/bad1.erl

	$i "Run the Xref plugin with exports_not_used, expect an error"
	$t ! $(MAKE) -C $(APP) xref XREF_CHECKS=exports_not_used $v

	$i "Run the Xref plugin with multiple checks"
	$t $(MAKE) -C $(APP) xref XREF_CHECKS="[undefined_function_calls, undefined_functions]" $v

	$i "Create a module with an undefined function call"
	$t printf "%s\n" \
		"-module(bad2)." \
		"-export([f/0])." \
		"f() -> this_module:does_not_exist()." \
		> $(APP)/src/bad2.erl

	$i "Run the Xref plugin with multiple checks, expect an error"
	$t ! $(MAKE) -C $(APP) xref XREF_CHECKS="[undefined_function_calls, undefined_functions]" $v

xref-check-informational: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Run the Xref plugin with module_use"
	$t $(MAKE) -C $(APP) xref XREF_CHECKS="{module_use, $(APP)_sup}" > $(APP)/output.txt

	$i "Confirm that the module was found"
	$t grep -q "\- $(APP)_app$$" $(APP)/output.txt

xref-query: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Run the Xref plugin with query XC (external calls)"
	$t $(MAKE) -C $(APP) xref q="XC" > $(APP)/output.txt

	$i "Confirm that the supervisor:start_link/3 call was found"
	$t grep -q "\- supervisor:start_link/3 called by $(APP)_sup:start_link/0$$" $(APP)/output.txt

xref-scope-apps: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a new library my_app"
	$t $(MAKE) -C $(APP) new-lib in=my_app $v

	$i "Create a module with an undefined function call inside my_app"
	$t printf "%s\n" \
		"-module(bad2)." \
		"-export([f/0])." \
		"f() -> this_module:does_not_exist()." \
		> $(APP)/apps/my_app/src/bad2.erl

	$i "Run the Xref plugin with apps in the scope, expect an error"
	$t ! $(MAKE) -C $(APP) xref XREF_SCOPE="app apps" $v

xref-scope-deps: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Bootstrap a new OTP library named my_dep inside $(APP)"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap-lib $v

	$i "Create a module with an undefined function call inside my_dep"
	$t printf "%s\n" \
		"-module(bad2)." \
		"-export([f/0])." \
		"f() -> this_module:does_not_exist()." \
		> $(APP)/my_dep/src/bad2.erl

	$i "Add my_dep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = cp $(CURDIR)/$(APP)/my_dep/\n"}' $(APP)/Makefile

ifdef LEGACY
	$i "Add my_dep to the applications key in the .app.src file"
	$t perl -ni.bak -e 'print;if ($$.==7) {print "\t\tmy_dep,\n"}' $(APP)/src/$(APP).app.src
endif

	$i "Run the Xref plugin with deps in the scope, expect an error"
	$t ! $(MAKE) -C $(APP) xref XREF_SCOPE="app deps" $v

xref-scope-otp: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Run the Xref plugin for module use with OTP in the scope"
	$t $(MAKE) -C $(APP) xref XREF_CHECKS="{module_use, asn1ct_pretty_format}" \
		XREF_SCOPE="app otp" > $(APP)/output.txt

	$i "Confirm that the asn1ct_pretty_format module use was analysed"
	$t grep -q "\- asn1ct_pretty_format$$" $(APP)/output.txt

xref-extra-apps: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Bootstrap a new OTP library named extra_app inside $(APP)"
	$t mkdir $(APP)/extra_app
	$t cp ../erlang.mk $(APP)/extra_app/
	$t $(MAKE) -C $(APP)/extra_app/ -f erlang.mk bootstrap-lib $v

	$i "Create a module in extra_app with a function call to $(APP)"
	$t printf "%s\n" \
		"-module(extra)." \
		"-export([f/0])." \
		"f() -> $(APP)_sup:init([])." \
		> $(APP)/extra_app/src/extra.erl

	$i "Build extra_app"
	$t $(MAKE) -C $(APP)/extra_app $v

	$i "Run the Xref plugin for application use with the extra app"
	$t $(MAKE) -C $(APP) xref XREF_CHECKS="{application_use, $(APP)}" \
		XREF_EXTRA_APP_DIRS="extra_app/" > $(APP)/output.txt

	$i "Confirm that the extra_app application call was found"
	$t grep -q "\- extra_app$$" $(APP)/output.txt

xref-extra-dirs: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Bootstrap a new OTP library named extra_dir inside $(APP)"
	$t mkdir $(APP)/extra_dir
	$t cp ../erlang.mk $(APP)/extra_dir/
	$t $(MAKE) -C $(APP)/extra_dir/ -f erlang.mk bootstrap-lib $v

	$i "Create a module in extra_dir with an undefined function call"
	$t printf "%s\n" \
		"-module(bad)." \
		"-export([f/0])." \
		"f() -> this_module:does_not_exist()." \
		> $(APP)/extra_dir/src/bad.erl

	$i "Build extra_dir"
	$t $(MAKE) -C $(APP)/extra_dir $v

	$i "Run the Xref plugin with the extra dir, expect an error"
	$t ! $(MAKE) -C $(APP) xref XREF_EXTRA_DIRS="extra_dir/ebin/" $v

xref-ignore-inline-fa: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a module with an undefined function call and an inline ignore"
	$t printf "%s\n" \
		"-module(bad)." \
		"-export([f/0])." \
		"-ignore_xref([{f,0}])." \
		"f() -> f_module:f_not_exist()." \
		> $(APP)/src/bad.erl

	$i "Run the Xref plugin, expect success"
	$t $(MAKE) -C $(APP) xref $v

xref-ignore-inline-mfa: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a module with undefined function calls and inline ignores"
	$t printf "%s\n" \
		"-module(bad)." \
		"-export([f/0, g/0])." \
		"-ignore_xref([{bad,f,0}])." \
		"-ignore_xref({g_module,g_not_exist,0})." \
		"f() -> f_module:f_not_exist()." \
		"g() -> g_module:g_not_exist()." \
		> $(APP)/src/bad.erl

	$i "Run the Xref plugin, expect success"
	$t $(MAKE) -C $(APP) xref $v

xref-ignore-inline-mod: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a module with undefined function calls and inline ignores"
	$t printf "%s\n" \
		"-module(bad)." \
		"-export([f/0, g/0])." \
		"-ignore_xref([?MODULE])." \
		"-ignore_xref(g_module)." \
		"f() -> f_module:f_not_exist()." \
		"g() -> g_module:g_not_exist()." \
		> $(APP)/src/bad.erl

	$i "Run the Xref plugin, expect success"
	$t $(MAKE) -C $(APP) xref $v

xref-ignore-project-wide-mfa: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a module with an undefined function call"
	$t printf "%s\n" \
		"-module(bad)." \
		"-export([f/0])." \
		"f() -> this_module:does_not_exist()." \
		> $(APP)/src/bad.erl

	$i "Run the Xref plugin with project-wide ignore, expect success"
	$t $(MAKE) -C $(APP) xref XREF_IGNORE="{bad,f,0}" $v

xref-ignore-project-wide-mod: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Create a module with an undefined function call"
	$t printf "%s\n" \
		"-module(bad)." \
		"-export([f/0])." \
		"f() -> this_module:does_not_exist()." \
		> $(APP)/src/bad.erl

	$i "Run the Xref plugin with project-wide ignore, expect success"
	$t $(MAKE) -C $(APP) xref XREF_IGNORE="[bad]" $v

xref-ignore-callbacks: init

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Run the Xref plugin for exports_not_used, expect success"
	$t $(MAKE) -C $(APP) xref XREF_CHECKS="exports_not_used" $v

	$i "Run the Xref plugin again with explicit ignoring of callbacks, expect success"
	$t $(MAKE) -C $(APP) xref XREF_CHECKS="exports_not_used" XREF_IGNORE_CALLBACKS=1 $v

	$i "Run the Xref plugin again without ignoring callbacks, expect an error"
	$t ! $(MAKE) -C $(APP) xref XREF_CHECKS="exports_not_used" XREF_IGNORE_CALLBACKS=0 $v
