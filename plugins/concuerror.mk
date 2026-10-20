# Copyright (c) Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifdef CONCUERROR_TESTS

.PHONY: concuerror distclean-concuerror

# Configuration

CONCUERROR_LOGS_DIR ?= $(CURDIR)/logs
CONCUERROR_OPTS ?=

CONCUERROR_GIT ?= https://github.com/parapluu/Concuerror
CONCUERROR_COMMIT ?= 0cbd2fc2acd7e1e01dc92c41c303f7f37b13dcca # master @ 2026-06-24

# Core targets.

check:: concuerror

ifndef KEEP_LOGS
distclean:: distclean-concuerror
endif

# Plugin-specific targets.

$(ERLANG_MK_TMP)/Concuerror/bin/concuerror: | $(ERLANG_MK_TMP)
	$(verbose) rm -rf $(ERLANG_MK_TMP)/Concuerror
	$(verbose) git clone -q -n -- $(CONCUERROR_GIT) $(ERLANG_MK_TMP)/Concuerror \
		&& cd $(ERLANG_MK_TMP)/Concuerror && git checkout -q $(CONCUERROR_COMMIT) \
		|| { rm -rf $(ERLANG_MK_TMP)/Concuerror; exit 1; }
	$(verbose) $(call dep_autopatch_fetch_rebar) \
		&& test -x $(ERLANG_MK_TMP)/rebar3/rebar3 \
		|| { rm -rf $(ERLANG_MK_TMP)/Concuerror; exit 1; }
	$(verbose) $(MAKE) -C $(ERLANG_MK_TMP)/Concuerror REBAR=$(ERLANG_MK_TMP)/rebar3/rebar3 \
		|| { rm -rf $(ERLANG_MK_TMP)/Concuerror; exit 1; }

$(CONCUERROR_LOGS_DIR):
	$(verbose) mkdir -p $(CONCUERROR_LOGS_DIR)

define concuerror_html_report
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Concuerror HTML report</title>
</head>
<body>
<h1>Concuerror HTML report</h1>
<p>Generated on $(concuerror_date)</p>
<ul>
$(foreach t,$(concuerror_targets),<li><a href="$(t).txt">$(t)</a></li>)
</ul>
</body>
</html>
endef

concuerror: $(addprefix concuerror-,$(subst :,-,$(CONCUERROR_TESTS)))
	$(eval concuerror_date := $(shell date))
	$(eval concuerror_targets := $^)
	$(verbose) $(call core_render,concuerror_html_report,$(CONCUERROR_LOGS_DIR)/concuerror.html)

define concuerror_target
.PHONY: concuerror-$1-$2

concuerror-$1-$2: test-build | $(ERLANG_MK_TMP)/Concuerror/bin/concuerror $(CONCUERROR_LOGS_DIR)
	$(ERLANG_MK_TMP)/Concuerror/bin/concuerror \
		--pa $(CURDIR)/ebin --pa $(TEST_DIR) \
		-o $(CONCUERROR_LOGS_DIR)/concuerror-$1-$2.txt \
		$$(CONCUERROR_OPTS) -m $1 -t $2
endef

$(foreach test,$(CONCUERROR_TESTS),$(eval $(call concuerror_target,$(firstword $(subst :, ,$(test))),$(lastword $(subst :, ,$(test))))))

distclean-concuerror:
	$(gen_verbose) rm -rf $(CONCUERROR_LOGS_DIR)

endif
