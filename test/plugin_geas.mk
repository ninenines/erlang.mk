# geas plugin.

GEAS_CASES = app 
GEAS_TARGETS = $(addprefix geas-,$(GEAS_CASES))
GEAS_CLEAN_TARGETS = $(addprefix clean-,$(GEAS_TARGETS))

.PHONY: geas clean-geas $(GEAS_CLEAN_TARGETS)

clean-geas: $(GEAS_CLEAN_TARGETS)

$(GEAS_CLEAN_TARGETS):
	rm -rf $(APP_TO_CLEAN)

geas: $(GEAS_TARGETS)

geas-app: build clean-geas-app

	$i "Bootstrap a new OTP application named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap $v
	$t mv -f $(APP)/Makefile $(APP)/Makefile.orig
	$t echo "BUILD_DEPS = geas " > $(APP)/Makefile
	$t echo "DEPS = cowboy " >> $(APP)/Makefile
	$t echo "DEP_PLUGINS = geas " >> $(APP)/Makefile
	$t cat $(APP)/Makefile.orig >> $(APP)/Makefile
	$t rm -f $(APP)/Makefile.orig

	$i "Run geas"
	$t $(MAKE) -C $(APP) geas 

	$i "Distclean the application"
	$t $(MAKE) -C $(APP) distclean $v
