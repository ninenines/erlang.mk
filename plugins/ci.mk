# Copyright (c) 2015-2017, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: ci ci-prepare ci-setup

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

ci:: $(addprefix ci-,$(CI_OTP) $(addsuffix -native,$(CI_HIPE)) $(addsuffix -erllvm,$(CI_ERLLVM)))

ci-prepare: $(addprefix $(KERL_INSTALL_DIR)/,$(CI_OTP) $(addsuffix -native,$(CI_HIPE)))

ci-setup::

ci-extra::

ci_verbose_0 = @echo " CI    " $(1);
ci_verbose = $(ci_verbose_$(V))

define ci_target
ci-$1: $(KERL_INSTALL_DIR)/$2
	$(verbose) $(MAKE) --no-print-directory clean
	$(ci_verbose) \
		PATH="$(KERL_INSTALL_DIR)/$2/bin:$(PATH)" \
		CI_OTP_RELEASE="$1" \
		CT_OPTS="-label $1" \
		CI_VM="$3" \
		$(MAKE) ci-setup tests
	$(verbose) $(MAKE) --no-print-directory ci-extra
endef

$(foreach otp,$(CI_OTP),$(eval $(call ci_target,$(otp),$(otp),otp)))
$(foreach otp,$(CI_HIPE),$(eval $(call ci_target,$(otp)-native,$(otp)-native,native)))
$(foreach otp,$(CI_ERLLVM),$(eval $(call ci_target,$(otp)-erllvm,$(otp)-native,erllvm)))

$(foreach otp,$(CI_OTP),$(eval $(call kerl_otp_target,$(otp))))
$(foreach otp,$(sort $(CI_HIPE) $(CI_ERLLLVM)),$(eval $(call kerl_hipe_target,$(otp))))

help::
	$(verbose) printf "%s\n" "" \
		"Continuous Integration targets:" \
		"  ci          Run '$(MAKE) tests' on all configured Erlang versions." \
		"" \
		"The CI_OTP variable must be defined with the Erlang versions" \
		"that must be tested. For example: CI_OTP = OTP-17.3.4 OTP-17.5.3"

endif
