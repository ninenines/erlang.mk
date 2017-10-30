# Copyright 2017, Stanislaw Klekot <dozzie@jarowit.net>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-sphinx sphinx

# Configuration.

SPHINX_BUILD ?= sphinx-build
SPHINX_SOURCE ?= doc
SPHINX_CONFIG ?=
SPHINX_BUILDERS ?= html
SPHINX_DOCTREES ?= $(ERLANG_MK_TMP)/sphinx.doctrees
SPHINX_OPTS ?=

#sphinx_html_opts =
#sphinx_html_output = html
#sphinx_man_opts =
#sphinx_man_output = man
#sphinx_latex_opts =
#sphinx_latex_output = latex

# Helpers.

sphinx_build_0 = @echo " SPHINX" $1; $(SPHINX_BUILD) -N -q
sphinx_build_1 = $(SPHINX_BUILD) -N
sphinx_build_2 = set -x; $(SPHINX_BUILD)
sphinx_build = $(sphinx_build_$(V))

define sphinx.build
$(call sphinx_build,$1) -b $1 -d $(SPHINX_DOCTREES) $(if $(SPHINX_CONFDIR),-c $(SPHINX_CONFDIR)) $(SPHINX_OPTS) $(sphinx_$1_opts) -- $(SPHINX_SOURCE) $(call sphinx.output,$1)

endef

define sphinx.output
$(if $(sphinx_$1_output),$(sphinx_$1_output),$1)
endef

# Targets.

ifneq ($(wildcard $(if $(SPHINX_CONFDIR),$(SPHINX_CONFDIR),$(SPHINX_SOURCE))/conf.py),)
docs:: sphinx
endif

distclean:: distclean-sphinx

# Plugin-specific targets.

sphinx:
	$(foreach B,$(SPHINX_BUILDERS),$(call sphinx.build,$B))

distclean-sphinx:
	$(gen_verbose) rm -rf $(filter-out $(SPHINX_SOURCE),$(foreach B,$(SPHINX_BUILDERS),$(call sphinx.output,$B)))
