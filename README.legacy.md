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

Xref plugin
------------

This plugin is available by default. It adds the following
target:

`xref` Erlang Xref Runner (inspired in rebar's rebar_xref)

The `XREF_CONFIG` variable specifies the location of the
configuration file which holds the checks to be applied.
If there is no `xref.config` all `xref` checks will be
applied to the binaries located in the `/ebin` directory.
