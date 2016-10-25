# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: ci ci-prepare ci-setup distclean-kerl

KERL ?= $(CURDIR)/kerl
export KERL

KERL_URL ?= https://raw.githubusercontent.com/yrashk/kerl/master/kerl

OTP_GIT ?= https://github.com/erlang/otp

CI_INSTALL_DIR ?= $(HOME)/erlang
CI_OTP ?=

ifeq ($(strip $(CI_OTP)),)
ci::
else
ci:: $(addprefix ci-,$(CI_OTP))

ci-prepare: $(addprefix $(CI_INSTALL_DIR)/,$(CI_OTP))

ci-setup::

ci_verbose_0 = @echo " CI    " $(1);
ci_verbose = $(ci_verbose_$(V))

define ci_target
ci-$(1): $(CI_INSTALL_DIR)/$(1)
	$(verbose) $(MAKE) --no-print-directory clean;
	$(ci_verbose) \
		PATH="$(CI_INSTALL_DIR)/$(1)/bin:$(PATH)" \
		CI_OTP_RELEASE="$(1)" \
		CT_OPTS="-label $(1)" \
		$(MAKE) ci-setup tests
endef

$(foreach otp,$(CI_OTP),$(eval $(call ci_target,$(otp))))

define ci_otp_target
ifeq ($(wildcard $(CI_INSTALL_DIR)/$(1)),)
$(CI_INSTALL_DIR)/$(1): $(KERL)
	$(KERL) build git $(OTP_GIT) $(1) $(1)
	$(KERL) install $(1) $(CI_INSTALL_DIR)/$(1)
endif
endef

$(foreach otp,$(CI_OTP),$(eval $(call ci_otp_target,$(otp))))

$(KERL):
	$(gen_verbose) $(call core_http_get,$(KERL),$(KERL_URL))
	$(verbose) chmod +x $(KERL)

help::
	$(verbose) printf "%s\n" "" \
		"Continuous Integration targets:" \
		"  ci          Run '$(MAKE) tests' on all configured Erlang versions." \
		"" \
		"The CI_OTP variable must be defined with the Erlang versions" \
		"that must be tested. For example: CI_OTP = OTP-17.3.4 OTP-17.5.3"

distclean:: distclean-kerl

distclean-kerl:
	$(gen_verbose) rm -rf $(KERL)
endif
