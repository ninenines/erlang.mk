# Copyright (c) Loïc Hoguin <essen@ninenines.eu>
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
	$(verbose) touch $(ERLANG_MK_RECURSIVE_TMP_LIST)
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
	$(verbose) mv $(ERLANG_MK_RECURSIVE_TMP_LIST).sorted $@
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

# Query dependencies recursively.

.PHONY: query-deps query-doc-deps query-rel-deps query-test-deps \
	query-shell-deps

QUERY ?= name fetch_method repo version

define query_target
$1: $2 clean-tmp-query.log
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) rm -f $4
endif
	$(verbose) $(foreach dep,$3,\
		echo $(PROJECT): $(foreach q,$(QUERY),$(call query_$(q),$(dep))) >> $4 ;)
	$(if $(filter-out query-deps,$1),,\
		$(verbose) set -e; for dep in $3 ; do \
			if grep -qs ^$$$$dep$$$$ $(ERLANG_MK_TMP)/query.log; then \
				:; \
			else \
				echo $$$$dep >> $(ERLANG_MK_TMP)/query.log; \
				$(MAKE) -C $(DEPS_DIR)/$$$$dep $$@ QUERY="$(QUERY)" IS_DEP=1 || true; \
			fi \
		done)
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) touch $4
	$(verbose) cat $4
endif
endef

clean-tmp-query.log:
ifeq ($(IS_DEP),)
	$(verbose) rm -f $(ERLANG_MK_TMP)/query.log
endif

$(eval $(call query_target,query-deps,$(ERLANG_MK_RECURSIVE_DEPS_LIST),$(BUILD_DEPS) $(DEPS),$(ERLANG_MK_QUERY_DEPS_FILE)))
$(eval $(call query_target,query-doc-deps,$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST),$(DOC_DEPS),$(ERLANG_MK_QUERY_DOC_DEPS_FILE)))
$(eval $(call query_target,query-rel-deps,$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST),$(REL_DEPS),$(ERLANG_MK_QUERY_REL_DEPS_FILE)))
$(eval $(call query_target,query-test-deps,$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST),$(TEST_DEPS),$(ERLANG_MK_QUERY_TEST_DEPS_FILE)))
$(eval $(call query_target,query-shell-deps,$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST),$(SHELL_DEPS),$(ERLANG_MK_QUERY_SHELL_DEPS_FILE)))

ERLANG_MK_TREE_FILE = $(ERLANG_MK_TMP)/tree.log

.PHONY: tree

ifeq ($(IS_APP)$(IS_DEP),)
tree:
	$(verbose) $(MAKE) --no-print-directory query-deps QUERY="name fetch_method repo version" >/dev/null
	$(verbose) $(call erlang,$(call dep_tree.erl,$(call core_native_path,$(ERLANG_MK_QUERY_DEPS_FILE)),$(call core_native_path,$(ERLANG_MK_TREE_FILE)),$(PROJECT)))
	$(verbose) cat $(ERLANG_MK_TREE_FILE)
else
tree: ;
endif

define dep_tree.erl
	{ok, Bin} = file:read_file("$1"),
	Edges = [case string:lexemes(Line, " ") of
		[Parent, Name, _, _, Version] ->
			{string:trim(Parent, trailing, ":"), Name, Version};
		_ ->
			io:format(standard_error, "bad query-deps line: ~ts~n", [Line]),
			halt(96)
	end || Line <- string:lexemes(Bin, "\n")],
	Root = unicode:characters_to_binary("$3"),
	Walk = fun Walk(Parent, Prefix, Seen) ->
		Children = [{N, V} || {P, N, V} <- Edges, P =:= Parent],
		Draw = fun Draw([], SeenAcc) ->
				{[], SeenAcc};
			Draw([{Name, Version}|Rest], SeenAcc) ->
				{Branch, Pad} = case Rest of
					[] -> {"\x{2514}\x{2500}\x{2500} ", "    "};
					_ -> {"\x{251C}\x{2500}\x{2500} ", "\x{2502}   "}
				end,
				Line = [Prefix, Branch, Name, " ", Version, "\n"],
				{ChildIo, Seen1} = case lists:member(Name, SeenAcc) of
					true -> {[], SeenAcc};
					false -> Walk(Name, [Prefix, Pad], [Name|SeenAcc])
				end,
				{RestIo, Seen2} = Draw(Rest, Seen1),
				{[Line, ChildIo, RestIo], Seen2}
			end,
		Draw(Children, Seen)
	end,
	{Body, _} = Walk(Root, "", [Root]),
	case file:write_file("$2", unicode:characters_to_binary([Root, "\n", Body])) of
		ok -> halt(0);
		{error, Reason} ->
			io:format(standard_error, "Error: ~0p~n", [Reason]),
			halt(1)
	end
endef
