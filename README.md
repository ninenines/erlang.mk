erlang.mk
=========

Common Makefile rules for building and testing Erlang applications.

Usage
-----

Add the file `erlang.mk` to your project, then use the following base
Makefile:

``` Makefile
PROJECT = my_project

include erlang.mk
```

Dependencies
------------

Erlang projects often depend on other projects to run. Adding dependencies
to the Makefile is easy. You need to create the variable `DEPS` listing
the names of all the dependencies, along with one `dep_$(NAME)` variable
per dependency giving the git repository and commit to retrieve.

These variables should be defined before the include line.

``` Makefile
DEPS = cowboy bullet
dep_cowboy = https://github.com/extend/cowboy.git 0.8.4
dep_bullet = https://github.com/extend/bullet.git 0.4.1
```

They will always be compiled using the command `make`.

Options
-------

The following variables can be overriden:

`V` defines the verbosity of the commands. You can set it
to an empty value to make commands verbose.

`ERLC_OPTS` allows you to change the `erlc` compilation
options. You should always compile with at least the `+debug_info` set.

`COMPILE_FIRST` is a list of modules (not filenames) that should be
compiled before all others.

`DEPS_DIR` is the path to the directory where the dependencies are
downloaded to. It defaults to `deps`. It will be propagated into
all the subsequent make calls, allowing all dependencies to use
the same folder as expected.

`CT_SUITES` is the list of common_test suites to run when you use
the `make tests` command. If your suite module is named `ponies_SUITE`
then you only need to put `ponies` in the list.

`PLT_APPS` is the list of applications to include when building the
`.plt` file for Dialyzer. You do not need to put `erts`, `kernel` or
`stdlib` in there because they will always be included. The applications
the project depends on will also be included.

`DIALYZER_OPTS` allows you to change the `dialyzer` options.

Extra targets
-------------

If you need more functionality out of your Makefile, you can add extra
targets after the include line.

Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
