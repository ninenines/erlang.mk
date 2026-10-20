# Copyright (c) Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-kerl

KERL_INSTALL_DIR ?= $(HOME)/erlang

ifeq ($(strip $(KERL)),)
KERL := $(ERLANG_MK_TMP)/kerl/kerl
endif

KERL_DIR = $(ERLANG_MK_TMP)/kerl

export KERL

KERL_GIT ?= https://github.com/kerl/kerl
KERL_COMMIT ?= e2e0ac5dc3dd8f49ce65e3db87f06986127b478a # 4.4.0

KERL_MAKEFLAGS ?=

OTP_GIT ?= https://github.com/erlang/otp

define kerl_otp_target
$(KERL_INSTALL_DIR)/$1: $(KERL)
	$(verbose) if [ ! -d $$@ ]; then \
		MAKEFLAGS="$(KERL_MAKEFLAGS)" $(KERL) build git $(OTP_GIT) $1 $1; \
		$(KERL) install $1 $(KERL_INSTALL_DIR)/$1; \
	fi
endef

$(KERL): | $(ERLANG_MK_TMP)
	$(verbose) rm -rf $(KERL_DIR)
	$(gen_verbose) git clone -q -n -- $(KERL_GIT) $(ERLANG_MK_TMP)/kerl \
		&& cd $(ERLANG_MK_TMP)/kerl && git checkout -q $(KERL_COMMIT) \
		&& chmod +x $(KERL) \
		|| { rm -rf $(KERL_DIR); exit 1; }

distclean:: distclean-kerl

distclean-kerl:
	$(gen_verbose) rm -rf $(KERL_DIR)

# Allow users to select which version of Erlang/OTP to use for a project.

ifneq ($(strip $(LATEST_ERLANG_OTP)),)
# In some environments it is necessary to filter out master.
ERLANG_OTP := $(notdir $(lastword $(sort\
	$(filter-out $(KERL_INSTALL_DIR)/master $(KERL_INSTALL_DIR)/OTP_R%,\
	$(filter-out %-rc1 %-rc2 %-rc3,$(wildcard $(KERL_INSTALL_DIR)/*[^-native]))))))
endif

ERLANG_OTP ?=

# Use kerl to enforce a specific Erlang/OTP version for a project.
ifneq ($(strip $(ERLANG_OTP)),)

export PATH := $(KERL_INSTALL_DIR)/$(ERLANG_OTP)/bin:$(PATH)
SHELL := env PATH=$(PATH) $(SHELL)
$(eval $(call kerl_otp_target,$(ERLANG_OTP)))

# Build Erlang/OTP only if it doesn't already exist.
ifeq ($(wildcard $(KERL_INSTALL_DIR)/$(ERLANG_OTP))$(BUILD_ERLANG_OTP),)
$(info Building Erlang/OTP $(ERLANG_OTP)... Please wait...)
$(shell $(MAKE) $(KERL_INSTALL_DIR)/$(ERLANG_OTP) ERLANG_OTP=$(ERLANG_OTP) BUILD_ERLANG_OTP=1 >&2)
endif

endif
