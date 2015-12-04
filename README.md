erlang.mk
=========

A build tool for Erlang that just works.

[Check out our upcoming user guide!](doc/src/guide/book.asciidoc)

The README only contains legacy documentation that was not moved to
the guide yet. Check there if you don't find what you're looking for.

Requirements
------------

`erlang.mk` requires GNU Make and expects to be ran in a standard
unix environment with Erlang installed and in the `$PATH`.

Common workflow
---------------

A common workflow when editing a file would be to run `make` regularly
to see if it compiles (or less often `make clean app` if you want to
recompile everything), followed by `make dialyze` to see if there are
any type errors and then `make tests` to run the test suites. The
result of the test runs can be browsed from the `logs/index.html` file.

Compiling and dependencies
--------------------------

[Check out our upcoming user guide!](doc/src/guide/book.asciidoc)

Releases
--------

[Check out our upcoming user guide!](doc/src/guide/book.asciidoc)

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

C/C++ compiler plugin
---------------------

[Check out our upcoming user guide!](doc/src/guide/book.asciidoc)

Common_test plugin
------------------

This plugin is available by default. It adds the following
target:

`ct` runs all test suites for this application.

There is nothing to configure to use it, simply create your
test suites in the `./test/` directory and erlang.mk will
figure everything out automatically.

You can override the list of suites that will run when using
`make tests` by setting the `CT_SUITES` variable.

You can add extra `ct_run` options by defining the `CT_OPTS`
variable. For more information please see `erl -man ct_run`.

You can run an individual test suite by using the special `ct-*`
targets. For example if you have a common_test suite named `spdy`
and you want to run only this suite and not the others, you can
use the `make ct-spdy` command.

Dialyzer plugin
---------------

[Check out our upcoming user guide!](doc/src/guide/book.asciidoc)

EDoc plugin
-----------

This plugin is available by default.

EDoc options can be specified in Erlang format by defining
the `EDOC_OPTS` variable. For more information please see
`erl -man edoc`.

Elvis plugin
------------

This plugin is available by default. It adds the following
target:

`elvis` runs Elvis style checker for this application.

The `ELVIS_CONFIG` variable specifies the location of the
configuration file which holds the rules to be applied.
If there's no `elvis.config` file the default one will be
downloaded. When the `ELVIS` variable points to a non-existing
file then the `elvis` executable will be downloaded as well.
Any other option should go in the `ELVIS_OPTS` variable.

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

EUnit plugin
------------

This plugin is available by default. It adds the following
target:

`eunit` which runs all the EUnit tests found in `ebin` or
the test directory specified in `TEST_DIR`.

`EUNIT_OPTS` can be used to specify EUnit-specific options
(e.g. `verbose`) that will be used when calling
`eunit:test/2`. This configuration parameter is empty
by default.. Note that EUnit options are specified as
a comma-separated list of options.

Relx plugin
-----------

[Check out our upcoming user guide!](doc/src/guide/book.asciidoc)

Shell plugin
------------

[Check out our upcoming user guide!](doc/src/guide/book.asciidoc)

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

Contributing
------------

[Check out our upcoming user guide!](doc/src/guide/book.asciidoc)

Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
