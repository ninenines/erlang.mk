# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: ci ci-prepare ci-setup distclean-kerl

CI_OTP ?=
CI_HIPE ?=
CI_ERLLVM ?=

ifeq ($(CI_VM),native)
ERLC_OPTS += +native
TEST_ERLC_OPTS += +native
else ifeq ($(CI_VM),erllvm)
ERLC_OPTS += +native +'{hipe, [to_llvm]}'
TEST_ERLC_OPTS += +native +'{hipe, [to_llvm]}'
endif

ifeq ($(strip $(CI_OTP) $(CI_HIPE) $(CI_ERLLVM)),)
ci::
else

ifeq ($(strip $(KERL)),)
KERL := $(ERLANG_MK_TMP)/kerl/kerl
endif

export KERL

KERL_GIT ?= https://github.com/kerl/kerl
KERL_COMMIT ?= master

KERL_MAKEFLAGS ?=

OTP_GIT ?= https://github.com/erlang/otp

CI_INSTALL_DIR ?= $(HOME)/erlang

ci:: $(addprefix ci-,$(CI_OTP) $(addsuffix -native,$(CI_HIPE)) $(addsuffix -erllvm,$(CI_ERLLVM)))

ci-prepare: $(addprefix $(CI_INSTALL_DIR)/,$(CI_OTP) $(addsuffix -native,$(CI_HIPE)))

ci-setup::

ci-extra::

ci_verbose_0 = @echo " CI    " $(1);
ci_verbose = $(ci_verbose_$(V))

define ci_target
ci-$1: $(CI_INSTALL_DIR)/$2
	$(verbose) $(MAKE) --no-print-directory clean
	$(ci_verbose) \
		PATH="$(CI_INSTALL_DIR)/$2/bin:$(PATH)" \
		CI_OTP_RELEASE="$1" \
		CT_OPTS="-label $1" \
		CI_VM="$3" \
		$(MAKE) ci-setup tests
	$(verbose) $(MAKE) --no-print-directory ci-extra
endef

$(foreach otp,$(CI_OTP),$(eval $(call ci_target,$(otp),$(otp),otp)))
$(foreach otp,$(CI_HIPE),$(eval $(call ci_target,$(otp)-native,$(otp)-native,native)))
$(foreach otp,$(CI_ERLLVM),$(eval $(call ci_target,$(otp)-erllvm,$(otp)-native,erllvm)))

define ci_otp_target
ifeq ($(wildcard $(CI_INSTALL_DIR)/$(1)),)
$(CI_INSTALL_DIR)/$(1): $(KERL)
	MAKEFLAGS="$(KERL_MAKEFLAGS)" $(KERL) build git $(OTP_GIT) $(1) $(1)
	$(KERL) install $(1) $(CI_INSTALL_DIR)/$(1)
endif
endef

$(foreach otp,$(CI_OTP),$(eval $(call ci_otp_target,$(otp))))

define ci_hipe_target
ifeq ($(wildcard $(CI_INSTALL_DIR)/$1-native),)
$(CI_INSTALL_DIR)/$1-native: $(KERL)
	KERL_CONFIGURE_OPTIONS=--enable-native-libs \
		MAKEFLAGS="$(KERL_MAKEFLAGS)" $(KERL) build git $(OTP_GIT) $1 $1-native
	$(KERL) install $1-native $(CI_INSTALL_DIR)/$1-native
endif
endef

$(foreach otp,$(sort $(CI_HIPE) $(CI_ERLLLVM)),$(eval $(call ci_hipe_target,$(otp))))

$(KERL):
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(gen_verbose) git clone --depth 1 $(KERL_GIT) $(ERLANG_MK_TMP)/kerl
	$(verbose) cd $(ERLANG_MK_TMP)/kerl && git checkout $(KERL_COMMIT)
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
