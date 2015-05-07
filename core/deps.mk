# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-deps distclean-pkg pkg-list pkg-search

# Configuration.

AUTOPATCH ?= edown gen_leader gproc
export AUTOPATCH

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR

ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(DEPS))

ifeq ($(filter $(DEPS_DIR),$(subst :, ,$(ERL_LIBS))),)
ifeq ($(ERL_LIBS),)
	ERL_LIBS = $(DEPS_DIR)
else
	ERL_LIBS := $(ERL_LIBS):$(DEPS_DIR)
endif
endif
export ERL_LIBS

PKG_FILE2 ?= $(CURDIR)/.erlang.mk.packages.v2
export PKG_FILE2

PKG_FILE_URL ?= https://raw.githubusercontent.com/ninenines/erlang.mk/master/packages.v2.tsv

# Core targets.

deps:: $(ALL_DEPS_DIRS)
	@for dep in $(ALL_DEPS_DIRS) ; do \
		if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ] ; then \
			$(MAKE) -C $$dep || exit $$? ; \
		else \
			echo "ERROR: No makefile to build dependency $$dep. Consider adding it to AUTOPATCH." ; \
			exit 1 ; \
		fi ; \
	done

distclean:: distclean-deps distclean-pkg

# Deps related targets.

define dep_autopatch
	$(ERL) -eval " \
DepDir = \"$(DEPS_DIR)/$(1)/\", \
fun() -> \
	{ok, Conf} = case file:consult(DepDir ++ \"rebar.config\") of \
		{error, enoent} -> {ok, []}; Res -> Res end, \
	File = case lists:keyfind(deps, 1, Conf) of false -> []; {_, Deps} -> \
		[begin {Method, Repo, Commit} = case Repos of \
			{git, R} -> {git, R, master}; \
			{M, R, {branch, C}} -> {M, R, C}; \
			{M, R, {tag, C}} -> {M, R, C}; \
			{M, R, C} -> {M, R, C} \
		end, \
		io_lib:format(\"DEPS += ~s\ndep_~s = ~s ~s ~s~n\", [Name, Name, Method, Repo, Commit]) \
		end || {Name, _, Repos} <- Deps] \
	end, \
	First = case lists:keyfind(erl_first_files, 1, Conf) of false -> []; {_, Files} -> \
		Names = [[\" \", begin \"lre.\" ++ R = lists:reverse(F), lists:reverse(R) end] \
			 || \"src/\" ++ F <- Files], \
		io_lib:format(\"COMPILE_FIRST +=~s\n\", [Names]) \
	end, \
	ok = file:write_file(\"$(DEPS_DIR)/$(1)/Makefile\", [\"ERLC_OPTS = +debug_info\n\n\", File, First, \"\ninclude erlang.mk\"]) \
end(), \
AppSrcOut = \"$(DEPS_DIR)/$(1)/src/$(1).app.src\", \
AppSrcIn = case filelib:is_regular(AppSrcOut) of false -> \"$(DEPS_DIR)/$(1)/ebin/$(1).app\"; true -> AppSrcOut end, \
fun() -> \
	{ok, [{application, $(1), L}]} = file:consult(AppSrcIn), \
	L2 = case lists:keyfind(modules, 1, L) of {_, _} -> L; false -> [{modules, []}|L] end, \
	L3 = case lists:keyfind(vsn, 1, L2) of {vsn, git} -> lists:keyreplace(vsn, 1, L2, {vsn, \"git\"}); _ -> L2 end, \
	ok = file:write_file(AppSrcOut, io_lib:format(\"~p.~n\", [{application, $(1), L3}])) \
end(), \
case AppSrcOut of AppSrcIn -> ok; _ -> ok = file:delete(AppSrcIn) end, \
halt()."
endef

ifeq ($(V),0)
define dep_autopatch_verbose
	@echo " PATCH " $(1);
endef
endif

define dep_fetch
	if [ "$$$$VS" = "git" ]; then \
		git clone -n -- $$$$REPO $(DEPS_DIR)/$(1); \
		cd $(DEPS_DIR)/$(1) && git checkout -q $$$$COMMIT; \
	elif [ "$$$$VS" = "hg" ]; then \
		hg clone -U $$$$REPO $(DEPS_DIR)/$(1); \
		cd $(DEPS_DIR)/$(1) && hg update -q $$$$COMMIT; \
	elif [ "$$$$VS" = "svn" ]; then \
		svn checkout $$$$REPO $(DEPS_DIR)/$(1); \
	elif [ "$$$$VS" = "cp" ]; then \
		cp -R $$$$REPO $(DEPS_DIR)/$(1); \
	else \
		echo "Unknown or invalid dependency: $(1). Please consult the erlang.mk README for instructions." >&2; \
		exit 78; \
	fi
endef

define dep_target
$(DEPS_DIR)/$(1):
	@mkdir -p $(DEPS_DIR)
ifeq (,$(dep_$(1)))
	@if [ ! -f $(PKG_FILE2) ]; then $(call core_http_get,$(PKG_FILE2),$(PKG_FILE_URL)); fi
	@DEPPKG=$$$$(awk 'BEGIN { FS = "\t" }; $$$$1 == "$(1)" { print $$$$2 " " $$$$3 " " $$$$4 }' $(PKG_FILE2);); \
	VS=$$$$(echo $$$$DEPPKG | cut -d " " -f1); \
	REPO=$$$$(echo $$$$DEPPKG | cut -d " " -f2); \
	COMMIT=$$$$(echo $$$$DEPPKG | cut -d " " -f3); \
	$(call dep_fetch,$(1))
else
	@VS=$(word 1,$(dep_$(1))); \
	REPO=$(word 2,$(dep_$(1))); \
	COMMIT=$(word 3,$(dep_$(1))); \
	$(call dep_fetch,$(1))
endif
ifneq ($(filter $(1),$(AUTOPATCH)),)
	$(call dep_autopatch_verbose,$(1)) \
		$(call dep_autopatch,$(1)); \
		cd $(DEPS_DIR)/$(1)/ && ln -s ../../erlang.mk
endif
endef

$(foreach dep,$(DEPS),$(eval $(call dep_target,$(dep))))

distclean-deps:
	$(gen_verbose) rm -rf $(DEPS_DIR)

# Packages related targets.

$(PKG_FILE2):
	@$(call core_http_get,$(PKG_FILE2),$(PKG_FILE_URL))

pkg-list: $(PKG_FILE2)
	@cat $(PKG_FILE2) | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$3 "\n" \
		"Website:\t" $$5 "\n" \
		"Description:\t" $$6 "\n" }'

ifdef q
pkg-search: $(PKG_FILE2)
	@cat $(PKG_FILE2) | grep -i ${q} | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$3 "\n" \
		"Website:\t" $$5 "\n" \
		"Description:\t" $$6 "\n" }'
else
pkg-search:
	$(error Usage: make pkg-search q=STRING)
endif

ifeq ($(PKG_FILE2),$(CURDIR)/.erlang.mk.packages.v2)
distclean-pkg:
	$(gen_verbose) rm -f $(PKG_FILE2)
endif

help::
	@printf "%s\n" "" \
		"Package-related targets:" \
		"  pkg-list              List all known packages" \
		"  pkg-search q=STRING   Search for STRING in the package index"
