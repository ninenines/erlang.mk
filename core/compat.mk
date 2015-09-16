# Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: rebar.config

# We strip out -Werror because we don't want to fail due to
# warnings when used as a dependency.

define compat_convert_erlc_opt
$(if $(filter-out -Werror,$1),\
	$(if $(findstring +,$1),\
		$(shell echo $1 | cut -b 2-)))
endef

define compat_rebar_config
{deps, [$(call comma_list,$(foreach d,$(DEPS),\
	{$(call dep_name,$d),".*",{git,"$(call dep_repo,$d)","$(call dep_commit,$d)"}}))]}.
{erl_opts, [$(call comma_list,$(foreach o,$(ERLC_OPTS),$(call compat_convert_erlc_opt,$o)))]}.
endef

$(eval _compat_rebar_config = $$(compat_rebar_config))
$(eval export _compat_rebar_config)

rebar.config:
	$(gen_verbose) echo "$${_compat_rebar_config}" > rebar.config
