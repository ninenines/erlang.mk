# Core: Miscellaneous.
#
# The miscellaneous tests use the prefix "core-", not "core-misc-".

CORE_ELIXIR_TARGETS = $(call list_targets,core-elixir)

.PHONY: core-elixir $(CORE_ELIXIR_TARGETS)

core-elixir: $(CORE_ELIXIR_TARGETS)

# @todo This one doesn't work with LEGACY=1
core-elixir-test-project_library: init
	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure Makefile"
	$t echo "ERLC_OPTS += +debug_info +'{parse_transform,lager_transform}'" >> $(APP)/Makefile
	$t echo "DEPS += elixir" >> $(APP)/Makefile
	$t echo "DEPS += lager" >> $(APP)/Makefile
	$t echo "DEPS += jason" >> $(APP)/Makefile
	$t echo "DEPS += phoenix" >> $(APP)/Makefile
	$t echo "dep_elixir_commit = v1.17.3" >> $(APP)/Makefile
	$t echo "dep_lager = git https://github.com/erlang-lager/lager master" >> $(APP)/Makefile
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

	$i "Check modules aren't duplicated"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="$(filter-out erl,$(ERL)) -pa $(APP)/deps/*/ebin/ $(APP)/ebin/ $(APP)/apps/*/ebin/ -eval \" \
		{ok, Apps} = application:ensure_all_started('$(APP)'), \
		[begin \
			{ok, Mods} = application:get_key(App, modules), \
			true = lists:sort(Mods) =:= lists:usort(Mods) \
		end || App <- Apps], \
		halt()\""

core-elixir-test-project_system: init
# @todo
ifneq ($(shell which elixirc),)
	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure Makefile"
	$t echo "ERLC_OPTS += +debug_info +'{parse_transform,lager_transform}'" >> $(APP)/Makefile
	$t echo "DEPS += lager" >> $(APP)/Makefile
	$t echo "DEPS += jason" >> $(APP)/Makefile
	$t echo "DEPS += phoenix" >> $(APP)/Makefile
	$t echo "dep_lager = git https://github.com/erlang-lager/lager master" >> $(APP)/Makefile
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

	$i "Check modules aren't duplicated"
	$t $(MAKE) -C $(APP) shell SHELL_OPTS="$(filter-out erl,$(ERL)) -pa $(APP)/deps/*/ebin/ $(APP)/ebin/ $(APP)/apps/*/ebin/ -eval \" \
		{ok, Apps} = application:ensure_all_started('$(APP)'), \
		[begin \
			{ok, Mods} = application:get_key(App, modules), \
			true = lists:sort(Mods) =:= lists:usort(Mods) \
		end || App <- Apps], \
		halt()\""
else
	$i "Test depends on a System Install of Elixir, skipping."
endif

core-elixir-test-project-rel: init
	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure Makefile"
	$t echo "ERLC_OPTS += +debug_info +'{parse_transform,lager_transform}'" >> $(APP)/Makefile
	$t echo "DEPS += lager" >> $(APP)/Makefile
	$t echo "DEPS += jason" >> $(APP)/Makefile
	$t echo "DEPS += phoenix" >> $(APP)/Makefile
	$t echo "dep_lager = git https://github.com/erlang-lager/lager master" >> $(APP)/Makefile
	$t echo "dep_jason = git https://github.com/michalmuskala/jason.git master" >> $(APP)/Makefile
	$t echo "dep_phoenix = hex 1.7.2" >> $(APP)/Makefile
	$t echo "$$(grep -v 'include erlang.mk' $(APP)/Makefile)" > $(APP)/Makefile
	$t echo "include erlang.mk" >> $(APP)/Makefile

	$i "Make deps"
	$t $(MAKE) -C $(APP) deps $v

	$i "Check a release can be made"
	$t $(MAKE) -C $(APP) bootstrap-rel
	$t $(MAKE) -C $(APP) rel

core-elixir-nif: init
# @todo
ifneq ($(shell which cpp >/dev/null && echo '#include "sodium.h"' | cpp -H -o /dev/null 2>&1 | head -n1 | grep -v 'No such file or directory'),)
	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v

	$i "Configure Makefile"
	$t echo "DEPS += libsalty2" >> $(APP)/Makefile
	$t echo "dep_libsalty2 = git https://github.com/Ianleeclark/libsalty2.git b11e544" >> $(APP)/Makefile
	$t echo "$$(grep -v 'include erlang.mk' $(APP)/Makefile)" > $(APP)/Makefile
	$t echo "include erlang.mk" >> $(APP)/Makefile

	$i "Make deps"
	$t $(MAKE) -C $(APP) deps $v

	$i "Check libsalty2 has compiled"
	$t test -f $(APP)/deps/libsalty2/ebin/libsalty2.app
	$t test -f $(APP)/deps/libsalty2/priv/salty_nif.so
else
	$i "Test depends on libsodium-dev, skipping."
endif
