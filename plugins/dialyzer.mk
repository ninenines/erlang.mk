# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: plt distclean-plt dialyze

# Configuration.

DIALYZER_PLT ?= $(CURDIR)/.$(PROJECT).plt
export DIALYZER_PLT

PLT_APPS ?=
DIALYZER_DIRS ?= --src -r $(wildcard src) $(ALL_APPS_DIRS)
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

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
	Opts = binary:split(<<"$1">>, <<"-">>, [global]),
	Filtered = lists:reverse(lists:foldl(fun
		(O = <<"pa ", _/bits>>, Acc) -> [O|Acc];
		(O = <<"D ", _/bits>>, Acc) -> [O|Acc];
		(O = <<"I ", _/bits>>, Acc) -> [O|Acc];
		(_, Acc) -> Acc
	end, [], Opts)),
	io:format("~s~n", [[["-", O] || O <- Filtered]]),
	halt().
endef

$(DIALYZER_PLT): deps app
	$(verbose) dialyzer --build_plt --apps erts kernel stdlib $(PLT_APPS) $(OTP_DEPS) $(LOCAL_DEPS) $(DEPS)

plt: $(DIALYZER_PLT)

distclean-plt:
	$(gen_verbose) rm -f $(DIALYZER_PLT)

ifneq ($(wildcard $(DIALYZER_PLT)),)
dialyze:
else
dialyze: $(DIALYZER_PLT)
endif
	$(verbose) dialyzer --no_native `$(call erlang,$(call filter_opts.erl,$(ERLC_OPTS)))` $(DIALYZER_DIRS) $(DIALYZER_OPTS)
