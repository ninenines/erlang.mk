# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2015-2016, Jean-Sébastien Pédron <jean-sebastien@rabbitmq.com>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Fetch dependencies recursively (without building them).

.PHONY: fetch-deps fetch-doc-deps fetch-rel-deps fetch-test-deps \
	fetch-shell-deps

.PHONY: $(ERLANG_MK_RECURSIVE_DEPS_LIST) \
	$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST) \
	$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST) \
	$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST) \
	$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST)

fetch-deps: $(ERLANG_MK_RECURSIVE_DEPS_LIST)
fetch-doc-deps: $(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST)
fetch-rel-deps: $(ERLANG_MK_RECURSIVE_REL_DEPS_LIST)
fetch-test-deps: $(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST)
fetch-shell-deps: $(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST)

ifneq ($(SKIP_DEPS),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST):
	$(verbose) :> $@
else
# By default, we fetch "normal" dependencies. They are also included no
# matter the type of requested dependencies.
#
# $(ALL_DEPS_DIRS) includes $(BUILD_DEPS).

$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(LOCAL_DEPS_DIRS) $(ALL_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST): $(LOCAL_DEPS_DIRS) $(ALL_DEPS_DIRS) $(ALL_DOC_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST): $(LOCAL_DEPS_DIRS) $(ALL_DEPS_DIRS) $(ALL_REL_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST): $(LOCAL_DEPS_DIRS) $(ALL_DEPS_DIRS) $(ALL_TEST_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST): $(LOCAL_DEPS_DIRS) $(ALL_DEPS_DIRS) $(ALL_SHELL_DEPS_DIRS)

# Allow to use fetch-deps and $(DEP_TYPES) to fetch multiple types of
# dependencies with a single target.
ifneq ($(filter doc,$(DEP_TYPES)),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_DOC_DEPS_DIRS)
endif
ifneq ($(filter rel,$(DEP_TYPES)),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_REL_DEPS_DIRS)
endif
ifneq ($(filter test,$(DEP_TYPES)),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_TEST_DEPS_DIRS)
endif
ifneq ($(filter shell,$(DEP_TYPES)),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_SHELL_DEPS_DIRS)
endif

ERLANG_MK_RECURSIVE_TMP_LIST := $(abspath $(ERLANG_MK_TMP)/recursive-tmp-deps-$(shell echo $$PPID).log)

$(ERLANG_MK_RECURSIVE_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST): | $(ERLANG_MK_TMP)
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) rm -f $(ERLANG_MK_RECURSIVE_TMP_LIST)
endif
	$(verbose) set -e; for dep in $^ ; do \
		if ! grep -qs ^$$dep$$ $(ERLANG_MK_RECURSIVE_TMP_LIST); then \
			echo $$dep >> $(ERLANG_MK_RECURSIVE_TMP_LIST); \
			if grep -qs -E "^[[:blank:]]*include[[:blank:]]+(erlang\.mk|.*/erlang\.mk|.*ERLANG_MK_FILENAME.*)$$" \
			 $$dep/GNUmakefile $$dep/makefile $$dep/Makefile; then \
				$(MAKE) -C $$dep fetch-deps \
				 IS_DEP=1 \
				 ERLANG_MK_RECURSIVE_TMP_LIST=$(ERLANG_MK_RECURSIVE_TMP_LIST); \
			fi \
		fi \
	done
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) sort < $(ERLANG_MK_RECURSIVE_TMP_LIST) | \
		uniq > $(ERLANG_MK_RECURSIVE_TMP_LIST).sorted
	$(verbose) cmp -s $(ERLANG_MK_RECURSIVE_TMP_LIST).sorted $@ \
		|| mv $(ERLANG_MK_RECURSIVE_TMP_LIST).sorted $@
	$(verbose) rm -f $(ERLANG_MK_RECURSIVE_TMP_LIST).sorted
	$(verbose) rm $(ERLANG_MK_RECURSIVE_TMP_LIST)
endif
endif # ifneq ($(SKIP_DEPS),)

# List dependencies recursively.

.PHONY: list-deps list-doc-deps list-rel-deps list-test-deps \
	list-shell-deps

list-deps: $(ERLANG_MK_RECURSIVE_DEPS_LIST)
list-doc-deps: $(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST)
list-rel-deps: $(ERLANG_MK_RECURSIVE_REL_DEPS_LIST)
list-test-deps: $(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST)
list-shell-deps: $(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST)

list-deps list-doc-deps list-rel-deps list-test-deps list-shell-deps:
	$(verbose) cat $^

# Show all deps recursively: path, version & homepage
show_dep_path = { printf "%s ", "'$$dep'" }
show_dep_hex = /hex / { printf("%s https://hex.pm/packages/%s\n", $$4, "'$$(basename $$dep)'") }
show_dep_git = /git / { printf("%s %s\n", $$5, $$4) }
show_dep_git_rmq = /git_rmq / { printf("%s https://github.com/rabbitmq/%s\n", $$7, $$4) }
show-deps: $(ERLANG_MK_RECURSIVE_DEPS_LIST)
	$(verbose) for dep in $(DEPS_DIR)/* \
	; do \
	  grep --no-filename "^dep_$$(basename $$dep) " $(DEPS_DIR)/*/{makefile,Makefile,*.mk} \
	  | sort \
	  | uniq \
	  | awk '$(show_dep_path); $(show_dep_hex); $(show_dep_git); $(show_dep_git_rmq)' \
	; done

# For dep_rabbitmq_management = git https://github.com/rabbitmq/rabbitmq-management v3.8.2
# The current show-deps implementation produces the following output:
#
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/amqp_client master https://github.com/rabbitmq/rabbitmq-erlang-client
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/aten 0.5.2 https://hex.pm/packages/aten
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/cowboy 2.0.0 https://hex.pm/packages/cowboy
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/cowboy 2.6.1 https://hex.pm/packages/cowboy
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/cowlib 2.7.0 https://github.com/ninenines/cowlib
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/cowlib 2.7.0 https://hex.pm/packages/cowlib
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/credentials_obfuscation 1.1.0 https://hex.pm/packages/credentials_obfuscation
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/gen_batch_server 0.8.2 https://hex.pm/packages/gen_batch_server
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/goldrush 0.1.9 https://github.com/DeadZen/goldrush.git
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/jsx 2.9.0 https://hex.pm/packages/jsx
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/lager 3.8.0 https://hex.pm/packages/lager
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/observer_cli 1.4.4 https://github.com/zhongwencool/observer_cli
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/observer_cli 1.5.2 https://hex.pm/packages/observer_cli
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/ra 1.0.5 https://hex.pm/packages/ra
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbit master https://github.com/rabbitmq/rabbitmq-server
# + /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbit v3.8.2 https://github.com/rabbitmq/rabbitmq-server
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbit_common master https://github.com/rabbitmq/rabbitmq-common
# + /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbit_common v3.8.2 https://github.com/rabbitmq/rabbitmq-common
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_cli master https://github.com/rabbitmq/rabbitmq-cli
# + /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_cli v3.8.2 https://github.com/rabbitmq/rabbitmq-cli
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_codegen master https://github.com/rabbitmq/rabbitmq-codegen
# + /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_codegen v3.8.2 https://github.com/rabbitmq/rabbitmq-codegen
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_management master https://github.com/rabbitmq/rabbitmq-management
# + /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_management v3.8.2 https://github.com/rabbitmq/rabbitmq-management
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_management_agent master https://github.com/rabbitmq/rabbitmq-management-agent
# + /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_management_agent v3.8.2 https://github.com/rabbitmq/rabbitmq-management-agent
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_web_dispatch master https://github.com/rabbitmq/rabbitmq-web-dispatch
# + /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/rabbitmq_web_dispatch v3.8.2 https://github.com/rabbitmq/rabbitmq-web-dispatch
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/ranch 1.7.1 https://github.com/ninenines/ranch
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/ranch 1.7.1 https://hex.pm/packages/ranch
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/recon 2.5.0 https://hex.pm/packages/recon
# - /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/recon 2.5.0 https://hex.pm/packages/recon
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/stdout_formatter 0.2.2 https://hex.pm/packages/stdout_formatter
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/syslog 3.4.5 https://github.com/schlagert/syslog
#   /Users/gerhard/github.com/rabbitmq/LicenseFinder/features/fixtures/erlangmk/deps/sysmon_handler 1.2.0 https://hex.pm/packages/sysmon_handler
#
# - extra line
# + missing line
#
# Can you think of a better approach than the one that I am currenlty on?
