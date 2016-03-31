Old Erlang.mk documentation
===========================

This documentation reminds here until it gets moved to the
official documentation on http://erlang.mk/guide/.

Extending Erlang.mk
-------------------

You may add additional operations to them by using the double
colons. Make will run all targets sharing the same name when
invoked.

``` Makefile
clean::
	@rm anotherfile
```

You can enable verbose mode by calling Make with the variable
`V` set to 1.

``` bash
$ make V=1
```

Parallel execution
------------------

*Parallel execution is currently enabled (experimental).*

Parallel execution can be enabled through the use of the
`-j` option. The following output showcases concurrent
downloading of dependencies.

``` bash
$ make -j32
Cloning into '/home/essen/ninenines/cowboy/deps/ranch'...
Cloning into '/home/essen/ninenines/cowboy/deps/cowlib'...
```

The `-O` option will ensure that output from different
targets is grouped, which is particularly useful when
running tests with different frameworks at the same time.
The disadvantage of this option however is that there is
no output until the target is completed.

The``MAKEFLAGS` variable can be used to set it permanently
on your system. It can be set in your `.zshrc`, `.bashrc`
or equivalent file.

``` bash
MAKEFLAGS="-j32 -O"
```

EDoc plugin
-----------

This plugin is available by default.

EDoc options can be specified in Erlang format by defining
the `EDOC_OPTS` variable. For more information please see
`erl -man edoc`.

ErlyDTL plugin
--------------

This plugin is available by default. It adds automatic
compilation of ErlyDTL templates found in `templates/*.dtl`
or any subdirectory. 

By default it ignores names of subdirectories and compiles 
`a/b/templatename.dtl` into `templatename_dtl.beam`. To include 
subdirectories names in the compiled module name add 
`DTL_FULL_PATH=1` into your Makefile - `a/b/templatename.dtl`
will be compiled into `a_b_templatename_dtl.beam`.

Additional ErlyDTL options can be specified as a comma-separated list
by defining the `DTL_OPTS` variable. Those options will be prepended
to the options specified by the plugin itself.

Escript plugin
--------------

This plugin is available by default. It adds the following
target:

`escript` which creates a shell-executable archive named
the same as your `$(PROJECT)`, containing the following files
from your application and its dependencies:

* `*.beam`
* contents of `priv/`
* `sys.config` for your application

There are a number of optional configuration parameters:

* `ESCRIPT_NAME` if a different output file is required
* `ESCRIPT_COMMENT` to alter the comment line in the escript header
* `ESCRIPT_BEAMS` for the paths searched for `*.beam` files to include
* `ESCRIPT_SYS_CONFIG` defaults to `rel/sys.config`
* `ESCRIPT_EMU_ARGS` for the parameters used to start the VM
* `ESCRIPT_SHEBANG` for the line used by your shell to start `escript`
* `ESCRIPT_STATIC` for non-beam directories to be included as well

Refer to http://www.erlang.org/doc/man/escript.html for
more information on `escript` functionality in general.

Triq plugin
-----------

This plugin is available by default. It adds the following
target:

`triq` will check all the properties found in `ebin` or
the test directory specified in `TEST_DIR`.

You can use the `t` variable to give a specific module
or function to run, for example:

``` bash
$ make triq t=cow_http_hd
```

Or:

``` bash
$ make triq t=cow_http_hd:prop_parse_accept
```

Xref plugin
------------

This plugin is available by default. It adds the following
target:

`xref` Erlang Xref Runner (inspired in rebar's rebar_xref)

The `XREF_CONFIG` variable specifies the location of the
configuration file which holds the checks to be applied.
If there is no `xref.config` all `xref` checks will be
applied to the binaries located in the `/ebin` directory.
