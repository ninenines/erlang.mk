# Core: Miscellaneous.
#
# The miscellaneous tests use the prefix "core-", not "core-misc-".

CORE_ELIXIR_TARGETS = $(call list_targets,core-elixir)

.PHONY: core-elixir $(CORE_ELIXIR_TARGETS)

core-elixir: $(CORE_ELIXIR_TARGETS)

core-test-project: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure Makefile"
	$i echo "ERLC_OPTS += +debug_info +'{parse_transform,lager_transform}'" >> $(APP)/Makefile
	$i echo "DEPS += lager" >> $(APP)/Makefile
	$i echo "DEPS += jason" >> $(APP)/Makefile
	$i echo "DEPS += phoenix" >> $(APP)/Makefile
	$i echo "dep_jason = git https://github.com/michalmuskala/jason.git master" >> $(APP)/Makefile
	$i echo "dep_phoenix = hex 1.7.2" >> $(APP)/Makefile

	$i "Make deps"
	$t $(MAKE) -C $(APP) deps $v

	$i "Check deps have compiled"
	$t test -d $(APP)/deps/lager/ebin
	$t test -d $(APP)/deps/jason/ebin
	$t test -d $(APP)/deps/phoenix/ebin

	$i "Make the app"
	$t $(MAKE) -C $(APP) app $v

	$i "Get started apps"
	$t $(ERL) -pa $(APP)/ebin/ $(APP)/apps/*/ebin/ -eval " \
		{ok, Apps} = application:ensure_all_started(exte), 
		true = lists:member(lager, Apps),
		true = lists:member(jason, Apps),
		true = lists:member(phoenix, Apps),
		halt()"