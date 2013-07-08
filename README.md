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

You can also specify test-only dependencies. These dependencies will only
be downloaded when running `make tests`. The format is the same as above,
except the variable `TEST_DEPS` holds the list of test-only dependencies.

``` erlang
TEST_DEPS = ct_helper
dep_ct_helper = https://github.com/extend/ct_helper.git master
```

Please note that the test dependencies will only be compiled once
when they are fetched, unlike the normal dependencies.

Compiled files
--------------

The following files will be automatically compiled:

| Wildcard                 | Description                   |
| ------------------------ | ----------------------------- |
| `src/$(PROJECT).app.src` | OTP application resource file |
| `src/*.erl`              | Erlang source files           |
| `src/*.core`             | Core Erlang source files      |
| `src/*.xrl`              | Leex source files             |
| `src/*.yrl`              | Yecc source files             |
| `templates/*.dtl`        | ErlyDTL template files        |

Only the `.app.src` file and at least one `.erl` file are required.

Commands
--------

The following targets are defined:

| Targets      | Description                                  |
| ------------ | -------------------------------------------- |
| `all`        | Compile the application and all dependencies |
| `clean-all`  | Clean the application and all dependencies   |
| `app`        | Compile the application                      |
| `clean`      | Clean the application                        |
| `deps`       | Compile the dependencies                     |
| `clean-deps` | Clean the dependencies                       |
| `docs`       | Generate the Edoc documentation              |
| `clean-docs` | Clean the Edoc documentation                 |
| `tests`      | Run the common_test suites                   |
| `build-plt`  | Generate the PLT needed by Dialyzer          |
| `dialyze`    | Run Dialyzer on the application              |

Cleaning means removing all generated and temporary files.

Dependencies are fetched as soon as a command involving them is
invoked. This means that most of the targets will trigger a
dependency fetch. It is only done once per dependency.

The default target when calling `make` is `all`.

You can combine targets to perform many operations. For example, the
shell command `make clean app` will have the effect of recompiling
the application fully, without touching the dependencies.

A common workflow when editing a file would be to run `make` regularly
to see if it compiles (or less often `make clean app` if you want to
recompile everything), followed by `make dialyze` to see if there are
any type errors and then `make tests` to run the test suites. The
result of the test runs can be browsed from the `logs/index.html` file.

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

Defining a target before the include line will override the default
target `all`.

Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
