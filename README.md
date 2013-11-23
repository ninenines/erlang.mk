erlang.mk
=========

Common Makefile rules for building and testing Erlang applications.

Also features support for dependencies and a package index.

Requirements
------------

`erlang.mk` requires GNU Make and expects to be ran in a standard
unix environment with Erlang installed and in the `$PATH`.

`erlang.mk` uses `wget` for downloading the package index file
when the `pkg://` scheme is used.

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

They will always be compiled using the command `make`. If the dependency
does not feature a Makefile, then erlang.mk will be used for building.

You can also specify test-only dependencies. These dependencies will only
be downloaded when running `make tests`. The format is the same as above,
except the variable `TEST_DEPS` holds the list of test-only dependencies.

``` erlang
TEST_DEPS = ct_helper
dep_ct_helper = https://github.com/extend/ct_helper.git master
```

Please note that the test dependencies will only be compiled once
when they are fetched, unlike the normal dependencies.

Package index
-------------

A very basic package index is included with erlang.mk. You can list
all known packages with the `make pkg-list` command. You can search
a package with the `make pkg-search q=STRING` command, replacing
`STRING` with what you want to search. Use quotes around the string
if needed.

In addition, it is possible to specify dependencies in a simplified
manner if they exist in the package index. The above example could
instead read as:

``` Makefile
DEPS = cowboy bullet
dep_cowboy = pkg://cowboy 0.8.4
dep_bullet = pkg://bullet 0.4.1
```

erlang.mk will look inside the index for the proper URL and use it
for fetching the dependency.

All packages featured in the index are compatible with erlang.mk
with no extra work required.

Releases
--------

If a `relx.config` file is present, erlang.mk will download `relx`
automatically and build the release into the `_rel` folder. This
is the default command when the file exists.

No special configuration is required for this to work.

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
| `test_*`     | Run the common_test suite `*`                |
| `tests`      | Run all the common_test suites               |
| `build-plt`  | Generate the PLT needed by Dialyzer          |
| `dialyze`    | Run Dialyzer on the application              |
| `pkg-list`   | List packages in the index                   |
| `pkg-search` | Search for packages in the index             |
| `rel`        | Build a release                              |
| `clean-rel`  | Delete the previously built release          |

Cleaning means removing all generated and temporary files.

Dependencies are fetched as soon as a command involving them is
invoked. This means that most of the targets will trigger a
dependency fetch. It is only done once per dependency.

You can run an individual test suite by using the special `test_*`
targets. For example if you have a common_test suite named `spdy`
and you want to run only this suite and not the others, you can
use the `make test_spdy` command.

The default target when calling `make` is `all` when no `relx.config`
exists, and `rel` when it does exist.

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

`EDOC_OPTS` allows you to specify
[options](http://www.erlang.org/doc/man/edoc.html#run-3) to pass to
`edoc` when building the documentation. Notice: not all options are
documented in one place; follow the links to get to the options for
the various operations of the documentation generation.

`CT_SUITES` is the list of common_test suites to run when you use
the `make tests` command. If your suite module is named `ponies_SUITE`
then you only need to put `ponies` in the list.

`PLT_APPS` is the list of applications to include when building the
`.plt` file for Dialyzer. You do not need to put `erts`, `kernel` or
`stdlib` in there because they will always be included. The applications
the project depends on will also be included.

`DIALYZER_OPTS` allows you to change the `dialyzer` options.

`PKG_FILE` allows you to change the location of the package index file
on your system.

`PKG_FILE_URL` allows you to change the URL from which the package index
file is fetched.

`RELX_CONFIG` is the location of the `relx.config` file, if any.

`RELX` is the location of the `relx` executable for building releases.

`RELX_URL` is the location where `relx` can be downloaded if it is
not found locally.

`RELX_OPTS` is for setting relx in-line options, if any.

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
