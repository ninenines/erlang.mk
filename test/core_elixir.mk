# Core: Miscellaneous.
#
# The miscellaneous tests use the prefix "core-", not "core-misc-".

CORE_ELIXIR_TARGETS = $(call list_targets,core-elixir)

.PHONY: core-elixir $(CORE_ELIXIR_TARGETS)

core-elixir: $(CORE_ELIXIR_TARGETS)

core-elixir-test-project: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure Makefile"
	$t echo "ERLC_OPTS += +debug_info +'{parse_transform,lager_transform}'" >> $(APP)/Makefile
	$t echo "DEPS += lager" >> $(APP)/Makefile
	$t echo "DEPS += jason" >> $(APP)/Makefile
	$t echo "DEPS += phoenix" >> $(APP)/Makefile
	$t echo "dep_jason = git https://github.com/michalmuskala/jason.git master" >> $(APP)/Makefile
	$t echo "dep_phoenix = hex 1.7.2" >> $(APP)/Makefile
	$t echo "$$(grep -v 'include erlang.mk' $(APP)/Makefile)" > $(APP)/Makefile
	$t echo "include erlang.mk" >> $(APP)/Makefile

	$i "Make deps"
	$t $(MAKE) -C $(APP) deps $v

	$i "Check deps have compiled"
	$t test -d $(APP)/deps/lager/ebin
	$t test -d $(APP)/deps/jason/ebin
	$t test -d $(APP)/deps/phoenix/ebin

	$i "Make the app"
	$t $(MAKE) -C $(APP) app $v

	$i "Get started apps"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="$(filter-out erl,$(ERL)) -pa $(APP)/deps/*/ebin/ $(APP)/ebin/ $(APP)/apps/*/ebin/ -eval \" \
		{ok, Apps} = application:ensure_all_started('$(APP)'), \
		true = lists:member(lager, Apps), \
		true = lists:member(jason, Apps), \
		true = lists:member(phoenix, Apps), \
		halt()\""