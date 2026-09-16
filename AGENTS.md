# AGENTS.md

Erlang.mk is a build tool for Erlang that just works.

Erlang.mk is a file to be included in Makefile via
`include erlang.mk` that provides support for building
Erlang/OTP applications, releases, but also testing,
analysing and more.

Edit `core/*.mk` and `plugins/*.mk`. Never edit the
concatenated `erlang.mk` or `templates.mk`; both are
generated.

## Commands
- `make` - build `erlang.mk` from `build.config` instructions
- `make check c=$TEST V=3` - one test or one group (such as `core-app`)
- `make check p=$PKG V=3` - one built-in index package
- `make check hp=$PKG V=3` - one Hex.pm package
- `make docs` - build documentation (only when documentation was requested)

Always run `make` after changing sources. It is cheap.

Do not run bare `make check`, `make packages` or
`make hexpm-packages`. Those run the entire suite.
At most run one group target, which is all tests in
one test file.

`V` ranges from 0 to 4. Use `V=3` when diagnosing a
failure.

## Layout
- `build.config` - files concatenated into `erlang.mk` (order matters)
- `core/` - core .mk files
- `plugins/` - optional .mk files
- `index/` - optional .mk metadata for built-in packages
- `templates/` - bootstrap templates; `templates.mk` is generated from them
- `test/` - Make-based tests (per core/plugin file)
- `test/Makefile` - test harness
- `doc/src/guide/` - user guide (in Asciidoc)

New `core/` or `plugins/` files must be added to
`build.config` in the right block or `make` will
omit them. Core first, then plugins, then plugins
that enhance others (cover, sfx), then `core/plugins`,
then `core/deps-tools` last.

## Core

- `beam-cache.mk` - .beam file cache (to switch between app/test builds)
- `compat.mk` - compatibility with other build tools
- `core.mk` - core symbols
- `deps.mk` - packages and dependencies
- `deps-tools.mk` - tools around dependencies
- `docs.mk` - core documentation symbols
- `elixir.mk` - elixir builder and dependency handler
- `erlc.mk` - erlang builder
- `index.mk` - built-in packages symbols
- `kerl.mk` - integrated Kerl support
- `plugins.mk` - plugins symbols
- `rel.mk` - core release symbols
- `test.mk` - core test symbols

## Plugins

- `asciidoc.mk` - Asciidoc documentation builder
- `bootstrap.mk` - Templates and bootstrapping
- `ci.mk` - Integrated CI support
- `concuerror.mk` - Concuerror tests runner
- `cover.mk` - Cover support for test runners
- `c_src.mk` - NIF build
- `ct.mk` - Common Test runner
- `dialyzer.mk` - Dialyzer runner
- `edoc.mk` - Edoc documentation builder
- `erlydtl.mk` - ErlyDTL templates builder
- `escript.mk` - Escript builder
- `eunit.mk` - EUnit tests runner
- `hex.mk` - Hex.pm support
- `proper.mk` - PropEr tests runner
- `protobuffs.mk` - Protobuffs builder
- `relx.mk` - Relx release builder
- `sfx.mk` - SFX archives builder
- `shell.mk` - Integrated shell
- `sphinx.mk` - Sphinx documentation builder
- `syntastic.mk` - Utility symbols
- `triq.mk` - Triq tests runner
- `xref.mk` - XRef runner

## When doing any sort of development
Don't include unnecessary comments. Comments are only useful
when the code is not obvious.

Do not remove existing comments.

### When implementing features
Always include tests for both normal cases and for edge cases.

Do not include documentation unless requested.

### When fixing bugs
Always write one or more tests before modifying the code. There
must be at least one test that fails before and succeeds after.

If you are not able to write a failing test, abort and tell
the user about it. This does not apply to refactoring as in
the refactoring case existing tests are enough.

### When writing tests
Tests are Make recipes in `test/core_*.mk` and
`test/plugin_*.mk`. They bootstrap a temporary app,
run `make`, and assert files or command output.
There is no separate unit-test framework.

Put new tests in the existing file for the component
you touched. Group target ≈ file: `test/core_app.mk`
→ `core-app`, `test/plugin_ct.mk` → `ct`. Tests are
generally ordered alphabetically. Do not move tests
that are in the wrong place if any.

A test is a target named `prefix-description` whose
first prerequisite is `init`. `$(APP)` is derived from
the target name. Bootstrap with:

	mkdir $(APP)/
	cp ../erlang.mk $(APP)/
	$(MAKE) -C $(APP) -f erlang.mk bootstrap $v

Use `bootstrap-lib` for a library. Use `$i` for info,
`$t` for the command, `$v` for erlang.mk verbosity.
Use `$(SLEEP)` when asserting rebuilds via mtimes.

`list_targets` picks up new targets automatically if
they use the file's prefix. Exception: `test/core_misc.mk`
uses the `core-` prefix, not `core-misc-`.

Match the density and style of neighboring tests in
that file. Do not duplicate existing cases; add the
scenario that is not already there.

Cover both success and failure conditions (build
succeeds, rebuild skips, clean removes the right
files, bad input fails). Naive tests are fine if
there are also more subtle ones.

Be thorough, including limits and boundaries, with
success expected inside the allowed range and failure
outside.

### When you think you are done
Review the changes and ensure all changes are necessary.
You can review changes with `git diff`.
Discard any change that is not necessary, then run
the same tests again.
