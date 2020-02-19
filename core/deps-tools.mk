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

# List 'NAME 	FETCH_METHOD 	VERSION 	HOMEPAGE 	PATH' for all deps, recursively
.PHONY: list-deps-info
list-deps-info: $(ALL_DEPS_DIRS:%=%-list-deps-info)

silence_nothing_to_be_done_lines := grep -v 'Nothing to be done'
continue := true
%-list-deps-info: $(ERLANG_MK_RECURSIVE_DEPS_LIST)
	$(verbose) echo " DEPI 	$(notdir $*) 	WIP_fetch_method 	$(call dep_commit,$(notdir $*)) 	$(call dep_repo,$(notdir $*)) 	$*"
	$(verbose) ($(MAKE) --no-print-directory -C $* list-deps-info IS_DEP=1 \
		    | $(silence_nothing_to_be_done_lines)) \
		   || $(continue)
