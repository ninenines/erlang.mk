# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: plt distclean-plt dialyze

# Configuration.

DIALYZER_PLT ?= $(CURDIR)/.$(PROJECT).plt
export DIALYZER_PLT

PLT_APPS ?=
DIALYZER_DIRS ?= --src -r $(wildcard src) $(ALL_APPS_DIRS)
DIALYZER_OPTS ?= -Werror_handling -Wunmatched_returns # -Wunderspecs
DIALYZER_PLT_OPTS ?=

# Core targets.

check:: dialyze

distclean:: distclean-plt

help::
	$(verbose) printf "%s\n" "" \
		"Dialyzer targets:" \
		"  plt         Build a PLT file for this project" \
		"  dialyze     Analyze the project using Dialyzer"

# Plugin-specific targets.

define filter_opts.erl
	Opts = init:get_plain_arguments(),
	{Filtered, _} = lists:foldl(fun
		(O,                         {Os, true}) -> {[O|Os], false};
		(O = "-D",                  {Os, _})    -> {[O|Os], true};
		(O = [\\$$-, \\$$D, _ | _], {Os, _})    -> {[O|Os], false};
		(O = "-I",                  {Os, _})    -> {[O|Os], true};
		(O = [\\$$-, \\$$I, _ | _], {Os, _})    -> {[O|Os], false};
		(O = "-pa",                 {Os, _})    -> {[O|Os], true};
		(_,                         Acc)        -> Acc
	end, {[], false}, Opts),
	io:format("~s~n", [string:join(lists:reverse(Filtered), " ")]),
	halt().
endef

# DIALYZER_PLT is a variable understood directly by Dialyzer.
#
# We append the path to erts at the end of the PLT. This works
# because the PLT file is in the external term format and the
# function binary_to_term/1 ignores any trailing data.
$(DIALYZER_PLT): deps app
	$(eval DEPS_LOG := $(shell test -f $(ERLANG_MK_TMP)/deps.log && \
		while read p; do test -d $$p/ebin && echo $$p/ebin; done <$(ERLANG_MK_TMP)/deps.log))
	$(verbose) dialyzer --build_plt $(DIALYZER_PLT_OPTS) --apps \
		erts kernel stdlib $(PLT_APPS) $(OTP_DEPS) $(LOCAL_DEPS) $(DEPS_LOG) || test $$? -eq 2
	$(verbose) $(ERL) -eval 'io:format("~n~s~n", [code:lib_dir(erts)]), halt().' >> $@

plt: $(DIALYZER_PLT)

distclean-plt:
	$(gen_verbose) rm -f $(DIALYZER_PLT)

ifneq ($(wildcard $(DIALYZER_PLT)),)
dialyze: $(if $(filter --src,$(DIALYZER_DIRS)),,deps app)
	$(verbose) if ! tail -n1 $(DIALYZER_PLT) | \
		grep -q "^`$(ERL) -eval 'io:format("~s", [code:lib_dir(erts)]), halt().'`$$"; then \
		rm $(DIALYZER_PLT); \
		$(MAKE) plt; \
	fi
else
dialyze: $(DIALYZER_PLT)
endif
	$(verbose) dialyzer `$(ERL) \
		-eval "$(subst $(newline),,$(call escape_dquotes,$(call filter_opts.erl)))" \
		-extra $(ERLC_OPTS)` $(DIALYZER_DIRS) $(DIALYZER_OPTS) $(if $(wildcard ebin/),-pa ebin/)
