# Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: clean-c_src
# todo

# Configuration.

C_SRC_DIR = $(CURDIR)/c_src
C_SRC_ENV ?= $(C_SRC_DIR)/env.mk
C_SRC_OPTS ?=
C_SRC_OUTPUT ?= $(CURDIR)/priv/$(PROJECT).so

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -flat_namespace -undefined suppress -finline-functions -Wall -Wmissing-prototypes
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
endif

# Verbosity.

c_src_verbose_0 = @echo " C_SRC " $(?F);
c_src_verbose = $(appsrc_verbose_$(V))

# Targets.

ifeq ($(wildcard $(C_SRC_DIR)/Makefile),)

app:: $(C_SRC_ENV)
	@mkdir -p priv/
	$(c_src_verbose) $(CC) $(CFLAGS) $(C_SRC_DIR)/*.c -fPIC -shared -o $(C_SRC_OUTPUT) \
		-I $(ERTS_INCLUDE_DIR) $(C_SRC_OPTS)

$(C_SRC_ENV):
	erl -noshell -noinput -eval "file:write_file(\"$(C_SRC_ENV)\", \
		io_lib:format(\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/\", \
			[code:root_dir(), erlang:system_info(version)])), \
		erlang:halt()."

-include $(C_SRC_ENV)

else
ifneq ($(wildcard $(C_SRC_DIR)),)

app::
	$(MAKE) -C $(C_SRC_DIR)

clean::
	$(MAKE) -C $(C_SRC_DIR) clean

endif
endif

clean:: clean-c_src

clean-c_src:
	$(gen_verbose) rm -f $(C_SRC_ENV) $(C_SRC_OUTPUT)
