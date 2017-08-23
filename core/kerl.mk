# Copyright (c) 2015-2017, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-kerl

KERL_INSTALL_DIR ?= $(HOME)/erlang

ifeq ($(strip $(KERL)),)
KERL := $(ERLANG_MK_TMP)/kerl/kerl
endif

export KERL

KERL_GIT ?= https://github.com/kerl/kerl
KERL_COMMIT ?= master

KERL_MAKEFLAGS ?=

OTP_GIT ?= https://github.com/erlang/otp

define kerl_otp_target
ifeq ($(wildcard $(KERL_INSTALL_DIR)/$(1)),)
$(KERL_INSTALL_DIR)/$(1): $(KERL)
	MAKEFLAGS="$(KERL_MAKEFLAGS)" $(KERL) build git $(OTP_GIT) $(1) $(1)
	$(KERL) install $(1) $(KERL_INSTALL_DIR)/$(1)
endif
endef

define kerl_hipe_target
ifeq ($(wildcard $(KERL_INSTALL_DIR)/$1-native),)
$(KERL_INSTALL_DIR)/$1-native: $(KERL)
	KERL_CONFIGURE_OPTIONS=--enable-native-libs \
		MAKEFLAGS="$(KERL_MAKEFLAGS)" $(KERL) build git $(OTP_GIT) $1 $1-native
	$(KERL) install $1-native $(KERL_INSTALL_DIR)/$1-native
endif
endef

$(KERL):
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(gen_verbose) git clone --depth 1 $(KERL_GIT) $(ERLANG_MK_TMP)/kerl
	$(verbose) cd $(ERLANG_MK_TMP)/kerl && git checkout $(KERL_COMMIT)
	$(verbose) chmod +x $(KERL)

distclean:: distclean-kerl

distclean-kerl:
	$(gen_verbose) rm -rf $(KERL)

# Allow users to select which version of Erlang/OTP to use for a project.

ERLANG_OTP ?=
ERLANG_HIPE ?=

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

else
# Same for a HiPE enabled VM.
ifneq ($(strip $(ERLANG_HIPE)),)
export PATH := $(KERL_INSTALL_DIR)/$(ERLANG_HIPE)-native/bin:$(PATH)
SHELL := env PATH=$(PATH) $(SHELL)
$(eval $(call kerl_hipe_target,$(ERLANG_HIPE)))

# Build Erlang/OTP only if it doesn't already exist.
ifeq ($(wildcard $(KERL_INSTALL_DIR)/$(ERLANG_HIPE))$(BUILD_ERLANG_OTP),)
$(info Building HiPE-enabled Erlang/OTP $(ERLANG_OTP)... Please wait...)
$(shell $(MAKE) $(KERL_INSTALL_DIR)/$(ERLANG_HIPE) ERLANG_HIPE=$(ERLANG_HIPE) BUILD_ERLANG_OTP=1 >&2)
endif

endif
endif
