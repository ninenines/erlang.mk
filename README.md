erlang.mk
=========

Common Makefile rules for building and testing Erlang applications.

Also features support for dependencies and a package index.

Requirements
------------

`erlang.mk` requires GNU Make and expects to be ran in a standard
unix environment with Erlang installed and in the `$PATH`.

`erlang.mk` uses `wget` for downloading the package index file.

`erlang.mk` will NOT work if the path contains spaces. This is a
limitation of POSIX compatible make build tools.

Usage
-----

Add the file `erlang.mk` to your project, then use the following base
Makefile:

``` Makefile
PROJECT = my_project
include erlang.mk
```

Alternatively you can use the following command to generate a skeleton
of an OTP application:

``` bash
$ make -f erlang.mk bootstrap
```

To generate a skeleton of an OTP library:

``` bash
$ make -f erlang.mk bootstrap-lib
```

Finally if you are going to create a release of this project you may
want to also use the `bootstrap-rel` target.

You can combine targets to perform many operations. For example, the
shell command `make clean app` will have the effect of recompiling
the application fully, without touching the dependencies.

A common workflow when editing a file would be to run `make` regularly
to see if it compiles (or less often `make clean app` if you want to
recompile everything), followed by `make dialyze` to see if there are
any type errors and then `make tests` to run the test suites. The
result of the test runs can be browsed from the `logs/index.html` file.

Getting help
------------

You can use `make help` to get help about erlang.mk or its plugins.

Packages
--------

A package index functionality is included with erlang.mk.

To use a package, you simply have to add it to the `DEPS` variable
in your Makefile. For example this depends on Cowboy:

``` Makefile
PROJECT = my_project
DEPS = cowboy
include erlang.mk
```

If the project you want is not included in the package index, or if
you want a different version, a few options are available. You can
edit the package file and contribute to it by opening a pull request.
You can use a custom package file, in which case you will probably
want to set the `PKG_FILE2` variable to its location. Or you can
put the project information directly in the Makefile.

In the latter case you need to create a variable `dep_*` with the
asterisk replaced by the project name, for example `cowboy`. This
variable must contain three things: the fetching method used, the
URL and the version requested.

The following snippet overrides the Cowboy version required:

``` Makefile
DEPS = cowboy
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.0
```

They will always be compiled using the command `make`. If the dependency
does not feature a Makefile, then erlang.mk will be used for building.

You can also specify test-only dependencies. These dependencies will only
be downloaded when running `make tests`. The format is the same as above,
except the variable `TEST_DEPS` holds the list of test-only dependencies.

``` erlang
TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/extend/ct_helper.git master
```

Please note that the test dependencies will only be compiled once
when they are fetched, unlike the normal dependencies.

Releases
--------

If a `relx.config` file is present, erlang.mk will download `relx`
automatically and build the release into the `_rel` folder. This
is the default command when the file exists.

No special configuration is required for this to work.

Customization
-------------

A custom erlang.mk may be created by editing the `build.config`
file and then running `make`. Only the core package handling
and erlc support are required.

If you need more functionality out of your Makefile, you can add extra
targets after the include line, or create an erlang.mk plugin.

Defining a target before the include line will override the default
target `all`.

The rest of this README starts by listing the core functionality
and then details each plugin individually.

Core functionality
------------------

The following targets are standard:

`all` is equivalent to `deps app rel`.

`deps` fetches and compiles the dependencies.

`app` compiles the application.

`rel` builds the release.

`docs` generates the documentation.

`tests` runs the test suites.

`clean` deletes the output files.

`distclean` deletes the output files but also any intermediate
files that are usually worth keeping around to save time,
and any other files needed by plugins (for example the Dialyzer
PLT file).

`help` gives some help about using erlang.mk.

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
$ V=1 make
```

Core package functionality
--------------------------

The following targets are specific to packages:

`pkg-list` lists all packages in the index.

`pkg-search n=STRING` searches the index for STRING.

Packages are downloaded into `DEPS_DIR` (`./deps/` by default).

The package index file is downloaded from `PKG_FILE_URL`
and saved in `PKG_FILE2`.

Core compiler functionality
---------------------------

erlang.mk will automatically compile the OTP application
resource file found in `src/$(PROJECT).app.src` (do note it
requires an empty `modules` line); Erlang source files found
in `src/*.erl` or any subdirectory; Core Erlang source files
found in `src/*.core` or any subdirectory; Leex source files
found in `src/*.xrl` or any subdirectory; and Yecc source
files found in `src/*.yrl` or any subdirectory.

You can change compilation options by setting the `ERLC_OPTS`
variable. It takes the arguments that will then be passed to
`erlc`. For more information, please see `erl -man erlc`.

You can specify a list of modules to be compiled first using
the `COMPILE_FIRST` variable.

Bootstrap plugin
----------------

This plugin is available by default. It adds the following
targets:

`bootstrap` generates a skeleton of an OTP application.

`bootstrap-lib` generates a skeleton of an OTP library.

`bootstrap-rel` generates the files needed to build a release.

`new` generate a skeleton module based on one of the available
templates.

`list-templates` lists the available templates.

C compiler plugin
-----------------

This plugin is not included by default. It is meant to
simplify the management of projects that include C source
code, like NIFs.

If the file `$(C_SRC_DIR)/Makefile` exists, then the plugin
simply calls it when needed. Otherwise it tries to compile
it directly.

You can use a different directory than `./c_src` by setting
the `C_SRC_DIR` variable.

You can override the output file by setting the `C_SRC_OUTPUT`
variable.

You can override the temporary file containing information
about Erlang's environment by setting the `C_SRC_ENV` variable.
This file is automatically generated on first run.

Finally you can add extra compiler options using the
`C_SRC_OPTS` variable. You can also override the defaults
`CC` and `CFLAGS` if required.

Common_test plugin
------------------

This plugin is available by default.

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

This plugin is available by default. It adds the following
targets:

`plt` builds the PLT file for this application.

`dialyze` runs Dialyzer.

The PLT file is built in `./$(PROJECT).plt` by default.
You can override this location by setting the `DIALYZER_PLT`
variable.

The `PLT_APPS` variable lists the applications that will be
included in the PLT file. There is no need to specify `erts`,
`kernel`, `stdlib` or the project's dependencies here, as they
are automatically added.

Dialyzer options can be modified by defining the `DIALYZER_OPTS`
variable. For more information please see `erl -man dialyzer`.

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

Relx plugin
-----------

This plugin is available by default.

You can change the location of the `relx` executable
(downloaded automatically) by defining the `RELX` variable,
and the location of the configuration file by defining
the `RELX_CONFIG` variable.

The URL used to download `relx` can be overriden by setting
the `RELX_URL` variable.

You can change the generated releases location by setting
the `RELX_OUTPUT_DIR` variable. Any other option should go
in the `RELX_OPTS` variable.

If `RELX_OPTS` includes the `-o` option (instead of using
`RELX_OUTPUT_DIR`, then that option must be the first in
the list, otherwise erlang.mk will fail to find it and
will not be able to clean up the release directory.

Contributing
------------

You can contribute by providing feedback, creating patches,
adding packages to the index or new features as plugins.

To add a package to the index, please use the `pkg_add.sh`
script. To use it, first fork the repository, then please
follow the example below:

``` bash
$ git clone https://github.com/$YOURUSERNAME/erlang.mk
$ cd erlang.mk
$ ./pkg_add.sh cowboy git https://github.com/ninenines/cowboy 1.0.0 http://ninenines.eu "Small, fast and modular HTTP server."
$ git push origin master
```

Then open a pull request. The arguments given to the script
are, in order, the project name, the download method used,
the repository URL, the commit/tag/branch/version to pull,
a link to the package's website and finally its description.
Make sure to put double quotes around the description.

You can submit as many packages as you want in one pull
request as long as you follow the instructions above.

For patches or plugins, you have to edit the `core/*.mk`
or `plugins/*.mk` files and then run `make` to create an
updated `erlang.mk`. If you submit a new plugin, you also
need to add it to the `build.config` file.

Make sure to keep the commit title short, to have a single
commit per package/feature/fix and you're good to submit
a pull request! And again, please don't forget to run make
and to commit the updated erlang.mk or index files along
with your other changes. Thanks!

Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
