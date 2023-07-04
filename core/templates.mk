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
	$(eval n := $(in))
	$(verbose) mkdir -p $(APPS_DIR)/$p/src/
	$(verbose) $(call core_render,bs_apps_Makefile,$(APPS_DIR)/$p/Makefile)
	$(verbose) $(call core_render,tpl_supervisor,$(APPS_DIR)/$p/src/$p_sup.erl)
	$(verbose) $(call core_render,tpl_app,$(APPS_DIR)/$p/src/$p_app.erl)
ifdef LEGACY
	$(eval app := $(p)_app)
	$(verbose) $(call core_render,tpl_appsrc,$(APPS_DIR)/$p/src/$p.app.src)
endif

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
	$(verbose) $(call core_render,bs_apps_Makefile,$(APPS_DIR)/$p/Makefile)
ifdef LEGACY
	$(verbose) $(call core_render,tpl_appsrc,$(APPS_DIR)/$p/src/$p.app.src)
endif

new:
ifeq ($(wildcard src/)$(in),)
	$(error Error: src/ directory does not exist)
endif
ifndef t
	$(error Usage: $(MAKE) new t=TEMPLATE n=NAME [in=APP])
endif
ifndef n
	$(error Usage: $(MAKE) new t=TEMPLATE n=NAME [in=APP])
endif
	$(eval dir := $(if $(tpl_$(t)_dir),$(tpl_$(t)_dir),./src))
	$(eval ext := $(if $(tpl_$(t)_ext),$(tpl_$(t)_ext),.erl))
ifeq ($(wildcard $(dir)),)
	$(verbose) mkdir -p $(dir)
endif
ifdef in
	$(verbose) $(call core_render,tpl_$(t),$(APPS_DIR)/$(in)/$(dir)/$(n)$(ext))
else
	$(verbose) $(call core_render,tpl_$(t),$(dir)/$(n)$(ext))
endif

list-templates:
	$(verbose) @echo Available templates:
	$(verbose) printf "    %s\n" $(sort $(patsubst tpl_%,%,$(filter-out %_ext %_dir,$(filter tpl_%,$(.VARIABLES)))))