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

$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST): $(ALL_DEPS_DIRS) $(ALL_DOC_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST): $(ALL_DEPS_DIRS) $(ALL_REL_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST): $(ALL_DEPS_DIRS) $(ALL_TEST_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST): $(ALL_DEPS_DIRS) $(ALL_SHELL_DEPS_DIRS)

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

ERLANG_MK_RECURSIVE_TMP_LIST := $(abspath $(ERLANG_MK_TMP)/recursive-tmp-deps.log)

$(ERLANG_MK_RECURSIVE_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST):
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(verbose) rm -f $(ERLANG_MK_RECURSIVE_TMP_LIST)
endif
ifndef IS_APP
	$(verbose) for dep in $(ALL_APPS_DIRS) ; do \
		$(MAKE) -C $$dep $@ \
		 IS_APP=1 \
		 ERLANG_MK_RECURSIVE_TMP_LIST=$(ERLANG_MK_RECURSIVE_TMP_LIST) \
		 || exit $$?; \
	done
endif
	$(verbose) for dep in $^ ; do \
		if ! grep -qs ^$$dep$$ $(ERLANG_MK_RECURSIVE_TMP_LIST); then \
			echo $$dep >> $(ERLANG_MK_RECURSIVE_TMP_LIST); \
			if grep -qs -E "^[[:blank:]]*include[[:blank:]]+(erlang\.mk|.*/erlang\.mk)$$" \
			 $$dep/GNUmakefile $$dep/makefile $$dep/Makefile; then \
				$(MAKE) -C $$dep fetch-deps \
				 IS_DEP=1 \
				 ERLANG_MK_RECURSIVE_TMP_LIST=$(ERLANG_MK_RECURSIVE_TMP_LIST) \
				 || exit $$?; \
			fi \
		fi \
	done
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) sort < $(ERLANG_MK_RECURSIVE_TMP_LIST) | uniq > $@
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
