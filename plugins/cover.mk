# Copyright 2015, Viktor Söderqvist <viktor@zuiderkwast.se>
# This file is part of erlang.mk and subject to the terms of the ISC License.

COVER_DIR = cover

# utility variables for representing special symbols
empty :=
space := $(empty) $(empty)
comma := ,

# Hook in coverage to eunit

ifdef COVER
ifdef EUNIT_RUN
EUNIT_RUN_BEFORE += -eval \
	'case cover:compile_beam_directory("ebin") of \
		{error, _} -> halt(1); \
		_ -> ok \
	end.'
EUNIT_RUN_AFTER += -eval 'cover:export("eunit.coverdata").'
endif
endif

# Hook in coverage to ct

ifdef COVER
ifdef CT_RUN

# All modules in 'ebin'
COVER_MODS = $(notdir $(basename $(shell echo ebin/*.beam)))

test-build:: ct.cover.spec

ct.cover.spec:
	@echo Cover mods: $(COVER_MODS)
	$(gen_verbose) printf "%s\n" \
		'{incl_mods,[$(subst $(space),$(comma),$(COVER_MODS))]}.' \
		'{export,"$(CURDIR)/ct.coverdata"}.' > $@

CT_RUN += -cover ct.cover.spec
endif
endif

# Core targets

ifdef COVER
ifneq ($(COVER_DIR),)
tests::
	@$(MAKE) make --no-print-directory cover-report
endif
endif

clean:: coverdata-clean

ifneq ($(COVER_DIR),)
distclean:: cover-clean
endif

help::
	@printf "%s\n" "" \
		"Cover targets:" \
		"  cover-report  Generate a HTML coverage report from previously collected" \
		"                cover data." \
		"" \
		"Cover-report is included in the 'tests' target by setting COVER=1." \
		"If you run 'ct' or 'eunit' separately with COVER=1, cover data is" \
		"collected but to generate a report you have to run 'cover-report'" \
		"afterwards."

# Plugin specific targets

.PHONY: coverdata-clean
coverdata-clean:
	$(gen_verbose) rm -f *.coverdata ct.cover.spec

# These are only defined if COVER_DIR is non-empty

ifneq ($(COVER_DIR),)

.PHONY: cover-clean cover-report

cover-clean: coverdata-clean
	$(gen_verbose) rm -rf $(COVER_DIR)

COVERDATA = $(wildcard *.coverdata)

ifeq ($(COVERDATA),)
cover-report:
else

# Modules which include eunit.hrl always contain one line without coverage
# because eunit defines test/0 which is never called. We compensate for this.
EUNIT_HRL_MODS = $(subst $(space),$(comma),$(shell \
	grep -e '^\s*-include.*include/eunit\.hrl"' src/*.erl \
	| sed 's/\.erl:.*//;s/^src\///' | uniq))

cover-report:
	$(gen_verbose) mkdir -p $(COVER_DIR)
	$(gen_verbose) $(ERL) -eval ' \
	$(foreach f,$(COVERDATA),cover:import("$(f)") == ok orelse halt(1),) \
	Ms = cover:imported_modules(), \
	[cover:analyse_to_file(M, "$(COVER_DIR)/" ++ atom_to_list(M) \
		++ ".COVER.html", [html])  || M <- Ms], \
	Report = [begin {ok, R} = cover:analyse(M, module), R end || M <- Ms], \
	EunitHrlMods = [$(EUNIT_HRL_MODS)], \
	Report1 = [{M, {Y, case lists:member(M, EunitHrlMods) of \
		true -> N - 1; false -> N end}} || {M, {Y, N}} <- Report], \
	TotalY = lists:sum([Y || {_, {Y, _}} <- Report1]), \
	TotalN = lists:sum([N || {_, {_, N}} <- Report1]), \
	TotalPerc = round(100 * TotalY / (TotalY + TotalN)), \
	{ok, F} = file:open("$(COVER_DIR)/index.html", [write]), \
	io:format(F, "<!DOCTYPE html><html>~n" \
		"<head><meta charset=\"UTF-8\">~n" \
		"<title>Coverage report</title></head>~n" \
		"<body>~n", []), \
	io:format(F, "<h1>Coverage</h1>~n<p>Total: ~p%</p>~n", [TotalPerc]),\
	io:format(F, "<table><tr><th>Module</th><th>Coverage</th></tr>~n", []), \
	[io:format(F, "<tr><td><a href=\"~p.COVER.html\">~p</a></td>" \
		"<td>~p%</td></tr>~n", \
		[M, M, round(100 * Y / (Y + N))]) || {M, {Y, N}} <- Report1], \
	How = "$(subst $(space),$(comma)$(space),$(basename $(COVERDATA)))", \
	Date = "$(shell date -u "+%Y-%m-%dT%H:%M:%SZ")", \
	io:format(F, "</table>~n" \
		"<p>Generated using ~s and erlang.mk on ~s.</p>~n" \
		"</body></html>", [How, Date]), \
	halt().'

endif
endif # ifneq ($(COVER_DIR),)
