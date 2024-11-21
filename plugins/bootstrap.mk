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

# Plugin-specific targets.

ifndef WS
ifdef SP
WS = $(subst a,,a $(wordlist 1,$(SP),a a a a a a a a a a a a a a a a a a a a))
else
WS = $(tab)
endif
endif

ifdef SP
define template_sp

# By default templates indent with a single tab per indentation
# level. Set this variable to the number of spaces you prefer:
SP = $(SP)

endef
else
template_sp =
endif

# @todo Additional template placeholders could be added.
subst_template = $(subst rel_root_dir,$(call core_relpath,$(dir $(ERLANG_MK_FILENAME)),$(APPS_DIR)/app),$(subst rel_deps_dir,$(call core_relpath,$(DEPS_DIR),$(APPS_DIR)/app),$(subst template_sp,$(template_sp),$(subst project_name,$p,$(subst template_name,$n,$1)))))

define core_render_template
	$(eval define _tpl_$(1)$(newline)$(call subst_template,$(tpl_$(1)))$(newline)endef)
	$(verbose) $(call core_render,_tpl_$(1),$2)
endef

bootstrap:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(if $(shell echo $p | LC_ALL=C grep -x "[a-z0-9_]*"),,\
		$(error Error: Invalid characters in the application name))
	$(eval n := $(PROJECT)_sup)
	$(verbose) $(call core_render_template,top_Makefile,Makefile)
	$(verbose) mkdir src/
ifdef LEGACY
	$(verbose) $(call core_render_template,application.app.src,src/$(PROJECT).app.src)
endif
	$(verbose) $(call core_render_template,application,src/$(PROJECT)_app.erl)
	$(verbose) $(call core_render_template,supervisor,src/$(PROJECT)_sup.erl)

bootstrap-lib:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(if $(shell echo $p | LC_ALL=C grep -x "[a-z0-9_]*"),,\
		$(error Error: Invalid characters in the application name))
	$(verbose) $(call core_render_template,top_Makefile,Makefile)
	$(verbose) mkdir src/
ifdef LEGACY
	$(verbose) $(call core_render_template,library.app.src,src/$(PROJECT).app.src)
endif

bootstrap-rel:
ifneq ($(wildcard relx.config),)
	$(error Error: relx.config already exists)
endif
ifneq ($(wildcard config/),)
	$(error Error: config/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(verbose) $(call core_render_template,relx.config,relx.config)
	$(verbose) mkdir config/
	$(verbose) $(call core_render_template,sys.config,config/sys.config)
	$(verbose) $(call core_render_template,vm.args,config/vm.args)
	$(verbose) awk '/^include erlang.mk/ && !ins {print "REL_DEPS += relx";ins=1};{print}' Makefile > Makefile.bak
	$(verbose) mv Makefile.bak Makefile

new-app:
ifndef in
	$(error Usage: $(MAKE) new-app in=APP)
endif
ifneq ($(wildcard $(APPS_DIR)/$in),)
	$(error Error: Application $in already exists)
endif
	$(eval p := $(in))
	$(if $(shell echo $p | LC_ALL=C grep -x "[a-z0-9_]*"),,\
		$(error Error: Invalid characters in the application name))
	$(eval n := $(in)_sup)
	$(verbose) mkdir -p $(APPS_DIR)/$p/src/
	$(verbose) $(call core_render_template,apps_Makefile,$(APPS_DIR)/$p/Makefile)
ifdef LEGACY
	$(verbose) $(call core_render_template,application.app.src,$(APPS_DIR)/$p/src/$p.app.src)
endif
	$(verbose) $(call core_render_template,application,$(APPS_DIR)/$p/src/$p_app.erl)
	$(verbose) $(call core_render_template,supervisor,$(APPS_DIR)/$p/src/$p_sup.erl)

new-lib:
ifndef in
	$(error Usage: $(MAKE) new-lib in=APP)
endif
ifneq ($(wildcard $(APPS_DIR)/$in),)
	$(error Error: Application $in already exists)
endif
	$(eval p := $(in))
	$(if $(shell echo $p | LC_ALL=C grep -x "[a-z0-9_]*"),,\
		$(error Error: Invalid characters in the application name))
	$(verbose) mkdir -p $(APPS_DIR)/$p/src/
	$(verbose) $(call core_render_template,apps_Makefile,$(APPS_DIR)/$p/Makefile)
ifdef LEGACY
	$(verbose) $(call core_render_template,library.app.src,$(APPS_DIR)/$p/src/$p.app.src)
endif

# These are not necessary because we don't expose those as "normal" templates.
BOOTSTRAP_TEMPLATES = apps_Makefile top_Makefile \
	application.app.src library.app.src application \
	relx.config sys.config vm.args

# Templates may override the path they will be written to when using 'new'.
# Only special template paths must be listed. Default is src/template_name.erl
# Substitution is also applied to the paths. Examples:
#
#tplp_top_Makefile = Makefile
#tplp_application.app.src = src/project_name.app.src
#tplp_application = src/project_name_app.erl
#tplp_relx.config = relx.config

# Erlang.mk bundles its own templates at build time into the erlang.mk file.

new:
	$(if $(t),,$(error Usage: $(MAKE) new t=TEMPLATE n=NAME [in=APP]))
	$(if $(n),,$(error Usage: $(MAKE) new t=TEMPLATE n=NAME [in=APP]))
	$(if $(tpl_$(t)),,$(error Error: $t template does not exist; try $(Make) list-templates))
	$(eval dest := $(if $(in),$(APPS_DIR)/$(in)/)$(call subst_template,$(if $(tplp_$(t)),$(tplp_$(t)),src/template_name.erl)))
	$(if $(wildcard $(dir $(dest))),,$(error Error: $(dir $(dest)) directory does not exist))
	$(if $(wildcard $(dest)),$(error Error: The file $(dest) already exists))
	$(eval p := $(PROJECT))
	$(call core_render_template,$(t),$(dest))

list-templates:
	$(verbose) @echo Available templates:
	$(verbose) printf "    %s\n" $(sort $(filter-out $(BOOTSTRAP_TEMPLATES),$(patsubst tpl_%,%,$(filter tpl_%,$(.VARIABLES)))))
