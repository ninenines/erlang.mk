# Copyright (c) 2017, Jean-Sébastien Pédron <jean-sebastien@rabbitmq.com>
# This file is contributed to erlang.mk and subject to the terms of the ISC License.

.PHONY: show-ERL_LIBS show-ERLC_OPTS show-TEST_ERLC_OPTS

show-ERL_LIBS:
	@echo $(ERL_LIBS)

show-ERLC_OPTS:
	@$(foreach opt,$(ERLC_OPTS) -pa ebin -I include,echo "$(opt)";)

show-TEST_ERLC_OPTS:
	@$(foreach opt,$(TEST_ERLC_OPTS) -pa ebin -I include,echo "$(opt)";)
