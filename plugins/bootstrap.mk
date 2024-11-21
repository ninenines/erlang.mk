# Copyright (c) 2014-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: bootstrap bootstrap-lib bootstrap-rel new list-templates

# Core targets.

help::
	$(verbose) printf "%s\n" "" \
		"Bootstrap targets:" \
		"  bootstrap          Generate a skeleton of an OTP application" \
		"  bootstrap-lib      Generate a skeleton of an OTP library" \
		"  bootstrap-rel      Generate the files needed to build a release" \
		"  new-app in=NAME    Create a new local OTP application NAME" \
		"  new-lib in=NAME    Create a new local OTP library NAME" \
		"  new t=TPL n=NAME   Generate a module NAME based on the template TPL" \
		"  new t=T n=N in=APP Generate a module NAME based on the template TPL in APP" \
		"  list-templates     List available templates"

# To prevent autocompletion issues with ZSH, we add "include erlang.mk"
# separately during the actual bootstrap.
define bs_Makefile
PROJECT = $p
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0
$(if $(SP),
# Whitespace to be used when creating files from templates.
SP = $(SP)
)
endef

define bs_apps_Makefile
PROJECT = $p
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0
$(if $(SP),
# Whitespace to be used when creating files from templates.
SP = $(SP)
)
# Make sure we know where the applications are located.
ROOT_DIR ?= $(call core_relpath,$(dir $(ERLANG_MK_FILENAME)),$(APPS_DIR)/app)
APPS_DIR ?= ..
DEPS_DIR ?= $(call core_relpath,$(DEPS_DIR),$(APPS_DIR)/app)

include $$(ROOT_DIR)/erlang.mk
endef

# Plugin-specific targets.

ifndef WS
ifdef SP
WS = $(subst a,,a $(wordlist 1,$(SP),a a a a a a a a a a a a a a a a a a a a))
else
WS = $(tab)
endif
endif

bootstrap:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(if $(shell echo $p | LC_ALL=C grep -x "[a-z0-9_]*"),,\
		$(error Error: Invalid characters in the application name))
	$(verbose) $(call core_render,bs_Makefile,Makefile)
	$(verbose) echo "include erlang.mk" >> Makefile
	$(verbose) mkdir src/
	$(eval n := $(PROJECT)_sup)
	$(verbose) $(call core_render,tpl_supervisor,src/$(PROJECT)_sup.erl)
	$(eval n := $(PROJECT)_app)
	$(eval sup := $(PROJECT)_sup)
	$(verbose) $(call core_render,tpl_app,src/$(PROJECT)_app.erl)
ifdef LEGACY
	$(eval n := $(PROJECT))
	$(verbose) $(call core_render,tpl_appsrc,src/$(PROJECT).app.src)
endif

bootstrap-lib:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(if $(shell echo $p | LC_ALL=C grep -x "[a-z0-9_]*"),,\
		$(error Error: Invalid characters in the application name))
	$(verbose) $(call core_render,bs_Makefile,Makefile)
	$(verbose) echo "include erlang.mk" >> Makefile
	$(verbose) mkdir src/
ifdef LEGACY
	$(eval n := $(PROJECT))
	$(verbose) $(call core_render,tpl_appsrc,src/$(PROJECT).app.src)
endif

bootstrap-rel:
ifneq ($(wildcard relx.config),)
	$(error Error: relx.config already exists)
endif
ifneq ($(wildcard config/),)
	$(error Error: config/ directory already exists)
endif
	$(eval n := $(PROJECT))
	$(verbose) $(call core_render,tpl_relx_config,relx.config)
	$(verbose) mkdir config/	$(verbose) $(call core_render,tpl_sys_config,config/sys.config)
	$(verbose) $(call core_render,tpl_vm_args,config/vm.args)
	$(verbose) awk '/^include erlang.mk/ && !ins {print "REL_DEPS += relx";ins=1};{print}' Makefile > Makefile.bak
	$(verbose) mv Makefile.bak Makefile