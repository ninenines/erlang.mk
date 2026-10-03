# Core: Dependency lock.

core_lock_TARGETS = $(call list_targets,core-lock)

.PHONY: core-lock $(core_lock_TARGETS)

core-lock: $(core_lock_TARGETS)

# Linux provides sha256sum. macOS provides shasum. FreeBSD provides sha256.
define sha256
if command -v sha256sum >/dev/null 2>&1; then sha256sum $(1); elif command -v shasum >/dev/null 2>&1; then shasum -a 256 $(1); else sha256 -q $(1); fi
endef

define lock_has_lines
while IFS= read -r line; do \
	[ -z "$$line" ] && continue; \
	grep -qxF "$$line" "$(APP)/lock.mk" || exit 1; \
done < $(1)
endef

core-lock-apps-dep-fetch-writes-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for my_dep"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add an application that depends on my_dep"
	$t mkdir -p $(APP)/apps/my_app
	$t cp ../erlang.mk $(APP)/apps/my_app/
	$t $(MAKE) -C $(APP)/apps/my_app -f erlang.mk bootstrap-lib $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/apps/my_app/Makefile

	$i "Build the application"
	$t $(MAKE) -C $(APP) apps $v

	$i "Check that the application dependency is recorded in the top-level lock"
	$t test -d $(APP)/deps/my_dep
	$t test ! -e $(APP)/apps/my_app/lock.mk
	$t test ! -e $(APP)/deps/my_dep/lock.mk
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk
	$t test `grep -c my_app $(APP)/lock.mk` -eq 0

	$i "Check that make lock enables the switch"
	$t $(MAKE) -C $(APP) lock $v
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk

core-lock-apps-locked-fetch-uses-sha: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two commits"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> README && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Add an application pinned to the older commit and lock it"
	$t mkdir -p $(APP)/apps/my_app
	$t cp ../erlang.mk $(APP)/apps/my_app/
	$t $(MAKE) -C $(APP)/apps/my_app -f erlang.mk bootstrap-lib $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) '"$$(cat $(APP)/sha-a)"'\n"}' $(APP)/apps/my_app/Makefile
	$t $(MAKE) -C $(APP) apps $v
	$t $(MAKE) -C $(APP) lock $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Point the application at master and build into an empty directory"
	$t sed -i.bak "s/$$(cat $(APP)/sha-a)/master/" $(APP)/apps/my_app/Makefile
	$t rm -rf $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) apps $v

	$i "Check that the locked commit is fetched and recorded again"
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`
	$t ! grep -qx two $(APP)/deps/my_dep/README
	$t $(call lock_has_lines,$(APP)/lock.mk.before)
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2

core-lock-cp-no-file: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add a copied dependency"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap-lib $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = cp $(CURDIR)/$(APP)/my_dep/\n"}' $(APP)/Makefile

	$i "Check that make lock leaves the copy in place and finds no lock file"
	$t ! $(MAKE) -C $(APP) --no-print-directory lock V=0 >$(APP)/lock.log 2>&1
	$t grep -q 'Error: lock.mk was not found. Fetch dependencies before locking.' $(APP)/lock.log
	$t grep -q 'Error 97' $(APP)/lock.log
	$t test -d $(APP)/deps/my_dep
	$t test ! -e $(APP)/lock.mk

core-lock-cp-with-git: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository and a directory to copy"
	$t mkdir $(APP)/git_repo $(APP)/copied_dep
	$t echo one > $(APP)/git_repo/README
	$t cp ../erlang.mk $(APP)/copied_dep/
	$t $(MAKE) -C $(APP)/copied_dep/ -f erlang.mk bootstrap-lib $v
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Fetch the Git dependency and the copy"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep copied_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\ndep_copied_dep = cp $(CURDIR)/$(APP)/copied_dep/\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Check that make lock enables the switch and leaves the copy unrecorded"
	$t $(MAKE) -C $(APP) lock $v
	$t test -d $(APP)/deps/my_dep
	$t test -d $(APP)/deps/copied_dep
	$t test ! -L $(APP)/deps/copied_dep
	$t test `grep -c '^dep_copied_dep ' $(APP)/lock.mk` -eq 0
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 1
	$t $(call lock_has_lines,$(APP)/lock.mk.before)

core-lock-git-distclean-keeps-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for my_dep"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add my_dep to the list of dependencies and fetch it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Check that distclean leaves lock.mk in place"
	$t $(MAKE) -C $(APP) distclean $v
	$t test ! -d $(APP)/deps
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk

core-lock-git-existing-checkout-unchanged: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for my_dep"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add my_dep to the list of dependencies, fetch it and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v
	$t git -C $(APP)/deps/my_dep rev-parse HEAD > $(APP)/sha-a
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Advance the remote and modify the existing checkout"
	$t echo two >> $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"
	$t echo local >> $(APP)/deps/my_dep/README

	$i "Check that a later fetch leaves the checkout and the lock alone"
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`
	$t test `git -C $(APP)/git_repo rev-parse HEAD` != `cat $(APP)/sha-a`
	$t grep -qx local $(APP)/deps/my_dep/README
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk

core-lock-git-fetch-directory-writes-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for my_dep"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Fetch only the dependency directory"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) $(abspath $(APP))/deps/my_dep $v

	$i "Check that the fetch wrote lock.mk"
	$t test -d $(APP)/deps/my_dep
	$t test ! -e $(APP)/.erlang.mk/lock/my_dep
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk

	$i "Finish the fetch and check that the lock still has one entry"
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 1

core-lock-git-fetch-writes-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for my_dep"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add my_dep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/Makefile

	$i "Fetch the dependency"
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that lock.mk records the fetched commit"
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk

	$i "Fetch again and check that the lock appends the same entry"
	$t rm -rf $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_my_dep_commit ' $(APP)/lock.mk` -eq 2

core-lock-git-lock-fetches-added: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create Git repositories for the two dependencies"
	$t mkdir $(APP)/git_repo_first $(APP)/git_repo_second
	$t echo first > $(APP)/git_repo_first/README
	$t echo second > $(APP)/git_repo_second/README
	$t cd $(APP)/git_repo_first && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"
	$t cd $(APP)/git_repo_second && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Lock the first dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = first_dep\ndep_first_dep = git file://$(abspath $(APP)/git_repo_first) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) lock $v
	$t git -C $(APP)/deps/first_dep rev-parse HEAD > $(APP)/sha-first

	$i "Add the second dependency and lock again"
	$t sed -i.bak 's/^DEPS = first_dep/DEPS = first_dep second_dep/' $(APP)/Makefile
	$t perl -ni.bak -e 'if (/^include erlang\.mk/) { print "dep_second_dep = git file://$(abspath $(APP)/git_repo_second) master\n" } print' $(APP)/Makefile
	$t $(MAKE) -C $(APP) lock $v

	$i "Check that the first dependency stayed put and the second was fetched"
	$t test `git -C $(APP)/deps/first_dep rev-parse HEAD` = `cat $(APP)/sha-first`
	$t test -d $(APP)/deps/second_dep
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_first_dep := git file://$(abspath $(APP)/git_repo_first) $$(cat $(APP)/sha-first)" \
		"dep_first_dep_commit := $$(cat $(APP)/sha-first)" \
		"dep_second_dep := git file://$(abspath $(APP)/git_repo_second) $$(git -C $(APP)/deps/second_dep rev-parse HEAD)" \
		"dep_second_dep_commit := $$(git -C $(APP)/deps/second_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 1

core-lock-git-lock-fetches-missing: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for my_dep"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add my_dep to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/Makefile

	$i "Check that make lock fetches the dependency and writes the switch"
	$t test ! -d $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) lock $v
	$t test -d $(APP)/deps/my_dep
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 1

core-lock-git-lock-fetches-partial: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create Git repositories for the two dependencies"
	$t mkdir $(APP)/git_repo_first $(APP)/git_repo_second
	$t echo first > $(APP)/git_repo_first/README
	$t echo second > $(APP)/git_repo_second/README
	$t cd $(APP)/git_repo_first && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"
	$t cd $(APP)/git_repo_second && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add both dependencies and fetch only the first"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = first_dep second_dep\ndep_first_dep = git file://$(abspath $(APP)/git_repo_first) master\ndep_second_dep = git file://$(abspath $(APP)/git_repo_second) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps DEPS=first_dep $v
	$t git -C $(APP)/deps/first_dep rev-parse HEAD > $(APP)/sha-first
	$t test ! -d $(APP)/deps/second_dep

	$i "Check that make lock fetches the missing dependency and keeps the first"
	$t $(MAKE) -C $(APP) lock $v
	$t test `git -C $(APP)/deps/first_dep rev-parse HEAD` = `cat $(APP)/sha-first`
	$t test -d $(APP)/deps/second_dep
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_first_dep := git file://$(abspath $(APP)/git_repo_first) $$(cat $(APP)/sha-first)" \
		"dep_first_dep_commit := $$(cat $(APP)/sha-first)" \
		"dep_second_dep := git file://$(abspath $(APP)/git_repo_second) $$(git -C $(APP)/deps/second_dep rev-parse HEAD)" \
		"dep_second_dep_commit := $$(git -C $(APP)/deps/second_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2

core-lock-git-lock-missing: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Check that make lock fails when lock.mk does not exist"
	$t ! $(MAKE) -C $(APP) --no-print-directory lock V=0 >$(APP)/lock-missing.log 2>&1
	$t grep -q 'Error: lock.mk was not found. Fetch dependencies before locking.' $(APP)/lock-missing.log
	$t grep -q 'Error 97' $(APP)/lock-missing.log
	$t test ! -e $(APP)/lock.mk

core-lock-git-lock-writes-switch: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for my_dep"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add my_dep to the list of dependencies and fetch it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that make lock writes the switch once"
	$t $(MAKE) -C $(APP) lock $v
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk
	$t $(MAKE) -C $(APP) lock $v
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 1

core-lock-git-lock-without-entry: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fetch my_dep and leave other_dep as a directory with no checkout"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep other_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\ndep_other_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/Makefile
	$t mkdir -p $(APP)/deps/other_dep
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Check that make lock enables the switch without a line for other_dep"
	$t $(MAKE) -C $(APP) lock $v
	$t test -d $(APP)/deps/my_dep
	$t test -d $(APP)/deps/other_dep
	$t test ! -e $(APP)/deps/other_dep/.git
	$t $(call lock_has_lines,$(APP)/lock.mk.before)
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 1
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 1
	$t test `grep -c '^dep_other_dep ' $(APP)/lock.mk` -eq 0

core-lock-git-locked-command-line-override: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two commits"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> README && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Fetch the older commit and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) '"$$(cat $(APP)/sha-a)"'\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`

	$i "Fetch again with a command-line commit"
	$t rm -rf $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) fetch-deps dep_my_dep_commit=master $v

	$i "Check that the command line wins and the lock records the new commit"
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `git -C $(APP)/git_repo rev-parse HEAD`
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` != `cat $(APP)/sha-a`
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t grep -qxF "dep_my_dep_commit := $$(cat $(APP)/sha-a)" $(APP)/lock.mk
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2

core-lock-git-locked-fetch-uses-sha: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two commits"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> README && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Fetch the older commit and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) '"$$(cat $(APP)/sha-a)"'\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Point the Makefile at master and fetch into an empty directory"
	$t sed -i.bak "s/$$(cat $(APP)/sha-a)/master/" $(APP)/Makefile
	$t rm -rf $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that the locked commit is fetched and recorded again"
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`
	$t test `git -C $(APP)/git_repo rev-parse HEAD` != `cat $(APP)/sha-a`
	$t $(call lock_has_lines,$(APP)/lock.mk.before)
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2

core-lock-git-locked-ignores-makefile-commit: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two commits"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> README && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Fetch the older commit and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) '"$$(cat $(APP)/sha-a)"'\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Point dep_my_dep_commit at master from the Makefile"
	$t perl -ni.bak -e 'print; if (/^dep_my_dep = /) { print "dep_my_dep_commit = master\n" }' $(APP)/Makefile
	$t rm -rf $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that the lock wins over the Makefile assignment"
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`
	$t test `git -C $(APP)/git_repo rev-parse HEAD` != `cat $(APP)/sha-a`
	$t $(call lock_has_lines,$(APP)/lock.mk.before)
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2

core-lock-git-locked-makefile-after-include: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two commits"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> README && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Fetch the older commit and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) '"$$(cat $(APP)/sha-a)"'\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v

	$i "Override the lock from below include erlang.mk"
	$t echo 'dep_my_dep_commit = master' >> $(APP)/Makefile
	$t rm -rf $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that an assignment after include erlang.mk leaves the locked commit"
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` != `git -C $(APP)/git_repo rev-parse HEAD`
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(cat $(APP)/sha-a)" \
		"dep_my_dep_commit := $$(cat $(APP)/sha-a)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c "dep_my_dep_commit := $$(cat $(APP)/sha-a)" $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2

core-lock-git-parallel-fetch: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create two Git repositories"
	$t mkdir $(APP)/git_repo_first $(APP)/git_repo_second
	$t echo first > $(APP)/git_repo_first/README
	$t echo second > $(APP)/git_repo_second/README
	$t cd $(APP)/git_repo_first && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"
	$t cd $(APP)/git_repo_second && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Fetch both dependencies in parallel"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = first_dep second_dep\ndep_first_dep = git file://$(abspath $(APP)/git_repo_first) master\ndep_second_dep = git file://$(abspath $(APP)/git_repo_second) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) -j2 fetch-deps $v

	$i "Check that lock.mk has one block for each dependency"
	$t printf '%s\n' \
		"dep_first_dep := git file://$(abspath $(APP)/git_repo_first) $$(git -C $(APP)/deps/first_dep rev-parse HEAD)" \
		"dep_first_dep_commit := $$(git -C $(APP)/deps/first_dep rev-parse HEAD)" \
		"dep_second_dep := git file://$(abspath $(APP)/git_repo_second) $$(git -C $(APP)/deps/second_dep rev-parse HEAD)" \
		"dep_second_dep_commit := $$(git -C $(APP)/deps/second_dep rev-parse HEAD)" \
		> $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^endif$$' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_first_dep ' $(APP)/lock.mk` -eq 1
	$t test `grep -c '^dep_second_dep ' $(APP)/lock.mk` -eq 1
	$t test ! -e $(APP)/.erlang.mk/lock/first_dep
	$t test ! -e $(APP)/.erlang.mk/lock/second_dep

core-lock-git-second-dep-keeps-first: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create Git repositories for the two dependencies"
	$t mkdir $(APP)/git_repo_first $(APP)/git_repo_second
	$t echo first > $(APP)/git_repo_first/README
	$t echo second > $(APP)/git_repo_second/README
	$t cd $(APP)/git_repo_first && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"
	$t cd $(APP)/git_repo_second && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-second && \
		echo more >> README && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Add both dependencies, pinning the second to its older commit"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = first_dep second_dep\ndep_first_dep = git file://$(abspath $(APP)/git_repo_first) master\ndep_second_dep = git file://$(abspath $(APP)/git_repo_second) '"$$(cat $(APP)/sha-second)"'\n"}' $(APP)/Makefile

	$i "Fetch the dependencies"
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t grep '^dep_first_dep' $(APP)/lock.mk > $(APP)/first_dep.lock

	$i "Point the second dependency at master and fetch it again"
	$t sed -i.bak "s/$$(cat $(APP)/sha-second)/master/" $(APP)/Makefile
	$t rm -rf $(APP)/deps/second_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that the first dependency is unchanged and the second commit was appended"
	$t grep '^dep_first_dep' $(APP)/lock.mk > $(APP)/first_dep.lock.after
	$t cmp $(APP)/first_dep.lock $(APP)/first_dep.lock.after
	$t test `git -C $(APP)/deps/second_dep rev-parse HEAD` != `cat $(APP)/sha-second`
	$t grep -qxF "dep_second_dep_commit := $$(cat $(APP)/sha-second)" $(APP)/lock.mk
	$t grep -qxF "dep_second_dep := git file://$(abspath $(APP)/git_repo_second) $$(git -C $(APP)/deps/second_dep rev-parse HEAD)" $(APP)/lock.mk
	$t grep -qxF "dep_second_dep_commit := $$(git -C $(APP)/deps/second_dep rev-parse HEAD)" $(APP)/lock.mk
	$t test `grep -c '^dep_second_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 3

core-lock-git-subfolder-fetch-writes-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with the application in a subfolder"
	$t mkdir -p $(APP)/git_repo/nested
	$t echo one > $(APP)/git_repo/nested/README
	$t echo top > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add my_dep as a git-subfolder dependency"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git-subfolder file://$(abspath $(APP)/git_repo) master nested\n"}' $(APP)/Makefile

	$i "Fetch the dependency"
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that lock.mk records the subfolder and the fetched commit"
	$t test -L $(APP)/deps/my_dep
	$t test `readlink $(APP)/deps/my_dep` = $(abspath $(APP))/.erlang.mk/git-subfolder/my_dep/nested
	$t test -f $(APP)/deps/my_dep/README
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git-subfolder file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/.erlang.mk/git-subfolder/my_dep rev-parse HEAD) nested" \
		"dep_my_dep_commit := $$(git -C $(APP)/.erlang.mk/git-subfolder/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk

	$i "Fetch again and check that the lock appends the same entry"
	$t rm -rf $(APP)/deps/my_dep $(APP)/.erlang.mk/git-subfolder/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_my_dep_commit ' $(APP)/lock.mk` -eq 2

core-lock-git-subfolder-keeps-git-dep: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git dependency and a git-subfolder dependency"
	$t mkdir -p $(APP)/git_repo_plain $(APP)/git_repo_folder/nested
	$t echo plain > $(APP)/git_repo_plain/README
	$t echo folder > $(APP)/git_repo_folder/nested/README
	$t cd $(APP)/git_repo_plain && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"
	$t cd $(APP)/git_repo_folder && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Fetch both dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = folder_dep plain_dep\ndep_folder_dep = git-subfolder file://$(abspath $(APP)/git_repo_folder) master nested\ndep_plain_dep = git file://$(abspath $(APP)/git_repo_plain) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that both dependencies are recorded"
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_folder_dep := git-subfolder file://$(abspath $(APP)/git_repo_folder) $$(git -C $(APP)/.erlang.mk/git-subfolder/folder_dep rev-parse HEAD) nested" \
		"dep_folder_dep_commit := $$(git -C $(APP)/.erlang.mk/git-subfolder/folder_dep rev-parse HEAD)" \
		"dep_plain_dep := git file://$(abspath $(APP)/git_repo_plain) $$(git -C $(APP)/deps/plain_dep rev-parse HEAD)" \
		"dep_plain_dep_commit := $$(git -C $(APP)/deps/plain_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2

core-lock-git-subfolder-lock-writes-switch: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with the application in a subfolder"
	$t mkdir -p $(APP)/git_repo/nested
	$t echo one > $(APP)/git_repo/nested/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Fetch the git-subfolder dependency and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git-subfolder file://$(abspath $(APP)/git_repo) master nested\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v

	$i "Check that the symlink is accepted and the switch is written once"
	$t test -L $(APP)/deps/my_dep
	$t test `readlink $(APP)/deps/my_dep` = $(abspath $(APP))/.erlang.mk/git-subfolder/my_dep/nested
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git-subfolder file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/.erlang.mk/git-subfolder/my_dep rev-parse HEAD) nested" \
		"dep_my_dep_commit := $$(git -C $(APP)/.erlang.mk/git-subfolder/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk
	$t $(MAKE) -C $(APP) lock $v
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 1

core-lock-git-subfolder-locked-fetch-uses-sha: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two commits"
	$t mkdir -p $(APP)/git_repo/nested
	$t echo one > $(APP)/git_repo/nested/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> nested/README && \
		git add nested/README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Fetch the older commit and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git-subfolder file://$(abspath $(APP)/git_repo) '"$$(cat $(APP)/sha-a)"' nested\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v

	$i "Point the Makefile at master and fetch into an empty directory"
	$t sed -i.bak "s/$$(cat $(APP)/sha-a)/master/" $(APP)/Makefile
	$t rm -rf $(APP)/deps/my_dep $(APP)/.erlang.mk/git-subfolder/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that the locked commit is fetched"
	$t test `git -C $(APP)/.erlang.mk/git-subfolder/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`
	$t test `git -C $(APP)/git_repo rev-parse HEAD` != `cat $(APP)/sha-a`
	$t test `readlink $(APP)/deps/my_dep` = $(abspath $(APP))/.erlang.mk/git-subfolder/my_dep/nested
	$t grep -qx one $(APP)/deps/my_dep/README
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git-subfolder file://$(abspath $(APP)/git_repo) $$(cat $(APP)/sha-a) nested" \
		"dep_my_dep_commit := $$(cat $(APP)/sha-a)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2

core-lock-git-subfolder-locked-makefile-after-include: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two commits"
	$t mkdir -p $(APP)/git_repo/nested
	$t echo one > $(APP)/git_repo/nested/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> nested/README && \
		git add nested/README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Fetch the older commit and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git-subfolder file://$(abspath $(APP)/git_repo) '"$$(cat $(APP)/sha-a)"' nested\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v

	$i "Override the lock from below include erlang.mk"
	$t echo 'dep_my_dep_commit = master' >> $(APP)/Makefile
	$t rm -rf $(APP)/deps/my_dep $(APP)/.erlang.mk/git-subfolder/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that an assignment after include erlang.mk leaves the locked commit"
	$t test `git -C $(APP)/.erlang.mk/git-subfolder/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`
	$t test `git -C $(APP)/.erlang.mk/git-subfolder/my_dep rev-parse HEAD` != `git -C $(APP)/git_repo rev-parse HEAD`
	$t grep -qx one $(APP)/deps/my_dep/README
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git-subfolder file://$(abspath $(APP)/git_repo) $$(cat $(APP)/sha-a) nested" \
		"dep_my_dep_commit := $$(cat $(APP)/sha-a)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c "dep_my_dep_commit := $$(cat $(APP)/sha-a)" $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2

core-lock-git-subfolder-locked-subfolder-after-include: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two subfolders"
	$t mkdir -p $(APP)/git_repo/nested $(APP)/git_repo/other
	$t echo one > $(APP)/git_repo/nested/README
	$t echo other > $(APP)/git_repo/other/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Fetch the nested subfolder and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git-subfolder file://$(abspath $(APP)/git_repo) master nested\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v
	$t git -C $(APP)/.erlang.mk/git-subfolder/my_dep rev-parse HEAD > $(APP)/sha-a

	$i "Point the subfolder at other from below include erlang.mk"
	$t echo "dep_my_dep = git-subfolder file://$(abspath $(APP)/git_repo) master other" >> $(APP)/Makefile
	$t rm -rf $(APP)/deps/my_dep $(APP)/.erlang.mk/git-subfolder/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that an assignment after include erlang.mk leaves the locked subfolder"
	$t test `git -C $(APP)/.erlang.mk/git-subfolder/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`
	$t test `readlink $(APP)/deps/my_dep` = $(abspath $(APP))/.erlang.mk/git-subfolder/my_dep/nested
	$t grep -qx one $(APP)/deps/my_dep/README
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git-subfolder file://$(abspath $(APP)/git_repo) $$(cat $(APP)/sha-a) nested" \
		"dep_my_dep_commit := $$(cat $(APP)/sha-a)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -cF "dep_my_dep := git-subfolder file://$(abspath $(APP)/git_repo) $$(cat $(APP)/sha-a) nested" $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2
	$t test `grep -c ' other$$' $(APP)/lock.mk` -eq 0

core-lock-git-submodule-fetch-skips-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository and add it as a submodule"
	$t mkdir $(APP)/my_dep
	$t echo one > $(APP)/my_dep/README
	$t cd $(APP)/my_dep && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"
	$t echo /my_dep > $(APP)/.gitignore
	$t mkdir $(APP)/deps
	$t cd $(APP) && \
		git init -q -b master && \
		git -c protocol.file.allow=always submodule -q add file://$(abspath $(APP)/my_dep) deps/my_dep && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Distclean and depend on the submodule"
	$t $(MAKE) -C $(APP) distclean $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git-submodule\n"}' $(APP)/Makefile

	$i "Fetch the dependency"
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that the fetch follows the gitlink and writes no lock file"
	$t test -d $(APP)/deps/my_dep
	$t test ! -L $(APP)/deps/my_dep
	$t test ! -e $(APP)/lock.mk
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `git -C $(APP) rev-parse HEAD:deps/my_dep`

	$i "Check that make lock does not invent an entry"
	$t ! $(MAKE) -C $(APP) --no-print-directory lock V=0 >$(APP)/lock.log 2>&1
	$t grep -q 'Error: lock.mk was not found. Fetch dependencies before locking.' $(APP)/lock.log
	$t grep -q 'Error 97' $(APP)/lock.log
	$t test ! -e $(APP)/lock.mk

core-lock-git-submodule-unlocked-follows-gitlink: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two commits and pin the submodule to the older one"
	$t mkdir $(APP)/my_dep
	$t echo one > $(APP)/my_dep/README
	$t cd $(APP)/my_dep && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> README && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"
	$t echo /my_dep > $(APP)/.gitignore
	$t mkdir $(APP)/deps
	$t cd $(APP) && \
		git init -q -b master && \
		git -c protocol.file.allow=always submodule -q add file://$(abspath $(APP)/my_dep) deps/my_dep && \
		git -C deps/my_dep checkout -q "$$(cat sha-a)" && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add . && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Distclean and fetch the submodule"
	$t $(MAKE) -C $(APP) distclean $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git-submodule\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `cat $(APP)/sha-a`

	$i "Move the gitlink to master and fetch into an empty directory"
	$t git -C $(APP)/deps/my_dep checkout -q master
	$t cd $(APP) && \
		git add deps/my_dep && \
		git commit -q --no-gpg-sign -m "Update submodule"
	$t rm -rf $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that the fetch follows the gitlink and writes no lock file"
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `git -C $(APP) rev-parse HEAD:deps/my_dep`
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` != `cat $(APP)/sha-a`
	$t grep -qx two $(APP)/deps/my_dep/README
	$t test ! -e $(APP)/lock.mk

core-lock-git-symlink-enables-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for my_dep and fetch it"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Replace the checkout with a symlink and lock"
	$t rm -rf $(APP)/deps/my_dep
	$t ln -s $(abspath $(APP)/git_repo) $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) lock $v

	$i "Check that make lock enables the switch and keeps the entry"
	$t test -L $(APP)/deps/my_dep
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 1
	$t $(call lock_has_lines,$(APP)/lock.mk.before)
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 1

core-lock-git-transitive-fetch-writes-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for leaf_dep"
	$t mkdir $(APP)/git_leaf
	$t echo leaf > $(APP)/git_leaf/README
	$t cd $(APP)/git_leaf && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Create a Git repository for mid_dep that depends on leaf_dep"
	$t mkdir $(APP)/git_mid
	$t cp ../erlang.mk $(APP)/git_mid/
	$t $(MAKE) -C $(APP)/git_mid -f erlang.mk bootstrap-lib $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = leaf_dep\ndep_leaf_dep = git file://$(abspath $(APP)/git_leaf) master\n"}' $(APP)/git_mid/Makefile
	$t rm -rf $(APP)/git_mid/.erlang.mk $(APP)/git_mid/deps $(APP)/git_mid/ebin
	$t rm -f $(APP)/git_mid/Makefile.bak
	$t cd $(APP)/git_mid && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add -A && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Create a Git repository for my_dep that depends on mid_dep"
	$t mkdir $(APP)/git_my
	$t cp ../erlang.mk $(APP)/git_my/
	$t $(MAKE) -C $(APP)/git_my -f erlang.mk bootstrap-lib $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = mid_dep\ndep_mid_dep = git file://$(abspath $(APP)/git_mid) master\n"}' $(APP)/git_my/Makefile
	$t rm -rf $(APP)/git_my/.erlang.mk $(APP)/git_my/deps $(APP)/git_my/ebin
	$t rm -f $(APP)/git_my/Makefile.bak
	$t cd $(APP)/git_my && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add -A && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Fetch my_dep"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_my) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that the resolved dependencies are recorded in the top-level lock"
	$t test -d $(APP)/deps/my_dep
	$t test -d $(APP)/deps/mid_dep
	$t test -d $(APP)/deps/leaf_dep
	$t test ! -e $(APP)/deps/my_dep/lock.mk
	$t test ! -e $(APP)/deps/mid_dep/lock.mk
	$t test ! -e $(APP)/deps/leaf_dep/lock.mk
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_leaf_dep := git file://$(abspath $(APP)/git_leaf) $$(git -C $(APP)/deps/leaf_dep rev-parse HEAD)" \
		"dep_leaf_dep_commit := $$(git -C $(APP)/deps/leaf_dep rev-parse HEAD)" \
		"dep_mid_dep := git file://$(abspath $(APP)/git_mid) $$(git -C $(APP)/deps/mid_dep rev-parse HEAD)" \
		"dep_mid_dep_commit := $$(git -C $(APP)/deps/mid_dep rev-parse HEAD)" \
		"dep_my_dep := git file://$(abspath $(APP)/git_my) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 3

core-lock-git-transitive-locked-fetch-uses-sha: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for inner_dep with two commits"
	$t mkdir $(APP)/git_inner
	$t echo one > $(APP)/git_inner/README
	$t cd $(APP)/git_inner && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> README && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Create a Git repository for my_dep pinned to the older commit"
	$t mkdir $(APP)/git_my
	$t cp ../erlang.mk $(APP)/git_my/
	$t $(MAKE) -C $(APP)/git_my -f erlang.mk bootstrap-lib $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = inner_dep\ndep_inner_dep = git file://$(abspath $(APP)/git_inner) '"$$(cat $(APP)/sha-a)"'\n"}' $(APP)/git_my/Makefile
	$t rm -rf $(APP)/git_my/.erlang.mk $(APP)/git_my/deps $(APP)/git_my/ebin
	$t rm -f $(APP)/git_my/Makefile.bak
	$t cd $(APP)/git_my && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add -A && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Fetch my_dep and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_my) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Point inner_dep at master and fetch into an empty directory"
	$t sed -i.bak "s/$$(cat $(APP)/sha-a)/master/" $(APP)/deps/my_dep/Makefile
	$t rm -rf $(APP)/deps/inner_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that the locked commit is fetched and recorded again"
	$t test `git -C $(APP)/deps/inner_dep rev-parse HEAD` = `cat $(APP)/sha-a`
	$t ! grep -qx two $(APP)/deps/inner_dep/README
	$t $(call lock_has_lines,$(APP)/lock.mk.before)
	$t test `grep -c '^dep_inner_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 1

core-lock-git-transitive-standalone-writes-own: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for inner_dep"
	$t mkdir $(APP)/git_inner
	$t echo one > $(APP)/git_inner/README
	$t cd $(APP)/git_inner && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Create a Git repository for my_dep that depends on inner_dep"
	$t mkdir $(APP)/git_my
	$t cp ../erlang.mk $(APP)/git_my/
	$t $(MAKE) -C $(APP)/git_my -f erlang.mk bootstrap-lib $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = inner_dep\ndep_inner_dep = git file://$(abspath $(APP)/git_inner) master\n"}' $(APP)/git_my/Makefile
	$t rm -rf $(APP)/git_my/.erlang.mk $(APP)/git_my/deps $(APP)/git_my/ebin
	$t rm -f $(APP)/git_my/Makefile.bak
	$t cd $(APP)/git_my && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add -A && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Fetch my_dep from the top-level project"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_my) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Fetch again from inside my_dep, without the inherited lock path"
	$t $(MAKE) -C $(APP)/deps/my_dep fetch-deps $v

	$i "Check that my_dep writes its own lock and leaves the top-level lock alone"
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk
	$t test -d $(APP)/deps/my_dep/deps/inner_dep
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_inner_dep := git file://$(abspath $(APP)/git_inner) $$(git -C $(APP)/deps/my_dep/deps/inner_dep rev-parse HEAD)" \
		"dep_inner_dep_commit := $$(git -C $(APP)/deps/my_dep/deps/inner_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/deps/my_dep/lock.mk

core-lock-git-unlock-removes-switch: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Check that make unlock does nothing when lock.mk is absent"
	$t $(MAKE) -C $(APP) unlock $v
	$t test ! -e $(APP)/lock.mk

	$i "Create a Git repository for my_dep"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Add my_dep to the list of dependencies and fetch it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.unlocked
	$t $(MAKE) -C $(APP) lock $v

	$i "Check that make unlock removes the switch and keeps the entries"
	$t $(MAKE) -C $(APP) unlock $v
	$t cmp $(APP)/lock.mk.unlocked $(APP)/lock.mk
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 0
	$t $(MAKE) -C $(APP) unlock $v
	$t cmp $(APP)/lock.mk.unlocked $(APP)/lock.mk

core-lock-git-unlocked-fetch-follows-makefile: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository with two commits"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests" && \
		git rev-parse HEAD > ../sha-a && \
		echo two >> README && \
		git add README && \
		git commit -q --no-gpg-sign -m "Update"

	$i "Fetch the older commit and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) '"$$(cat $(APP)/sha-a)"'\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v

	$i "Unlock, point the Makefile at master and fetch into an empty directory"
	$t $(MAKE) -C $(APP) unlock $v
	$t sed -i.bak "s/$$(cat $(APP)/sha-a)/master/" $(APP)/Makefile
	$t rm -rf $(APP)/deps/my_dep
	$t $(MAKE) -C $(APP) fetch-deps $v

	$i "Check that the fetch follows the Makefile and keeps the older pin"
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` = `git -C $(APP)/git_repo rev-parse HEAD`
	$t test `git -C $(APP)/deps/my_dep rev-parse HEAD` != `cat $(APP)/sha-a`
	$t grep -qxF "dep_my_dep_commit := $$(cat $(APP)/sha-a)" $(APP)/lock.mk
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 0
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^dep_my_dep ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2

core-lock-hex-checksum-mismatch: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fetch Cowlib 2.12.1 and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = hex 2.12.1\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v
	$t $(MAKE) -C $(APP) lock CI_ERLANG_MK= $v

	$i "Replace the recorded checksum and fetch into an empty directory"
	$t sed -i.bak 's/^dep_cowlib_checksum := .*/dep_cowlib_checksum := 0000000000000000000000000000000000000000000000000000000000000000/' $(APP)/lock.mk
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before
	$t rm -rf $(APP)/deps/cowlib
	$t ! $(MAKE) -C $(APP) --no-print-directory fetch-deps CI_ERLANG_MK= V=0 >$(APP)/checksum.log 2>&1

	$i "Check that the checksum is rejected and the lock is left unchanged"
	$t grep -q 'Error: checksum mismatch for cowlib.' $(APP)/checksum.log
	$t grep -q 'Error 101' $(APP)/checksum.log
	$t test ! -e $(APP)/deps/cowlib
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk

	$i "Fetch again and check that the checksum is still rejected"
	$t ! $(MAKE) -C $(APP) --no-print-directory fetch-deps CI_ERLANG_MK= V=0 >$(APP)/checksum2.log 2>&1
	$t grep -q 'Error: checksum mismatch for cowlib.' $(APP)/checksum2.log
	$t test ! -e $(APP)/deps/cowlib
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk

core-lock-hex-checksum-tarball: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fetch Cowlib 2.12.1 into the cache and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = hex 2.12.1\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= CACHE_DEPS=1 $v
	$t $(MAKE) -C $(APP) lock CI_ERLANG_MK= CACHE_DEPS=1 $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Replace the cached archive"
	$t mkdir -p $(APP)/fake/src
	$t echo fake > $(APP)/fake/src/fake.erl
	$t tar -C $(APP)/fake -czf $(APP)/contents.tar.gz src
	$t tar -C $(APP) -cf $(APP)/fake.tar contents.tar.gz
	$t cp $(APP)/fake.tar $(CACHE_DIR)/hex/cowlib-2.12.1.tar
	$t rm -rf $(APP)/deps/cowlib

	$i "Check that the archive checksum is rejected"
	$t ! $(MAKE) -C $(APP) --no-print-directory fetch-deps CI_ERLANG_MK= CACHE_DEPS=1 V=0 >$(APP)/tarball.log 2>&1
	$t grep -q 'Error: checksum mismatch for cowlib.' $(APP)/tarball.log
	$t test ! -e $(APP)/deps/cowlib
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk

core-lock-hex-download-failure: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fetch Cowlib 2.12.1"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = hex 2.12.1\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before
	$t rm -rf $(APP)/deps/cowlib

	$i "Request a version Hex does not have"
	$t sed -i.bak 's/^dep_cowlib = hex 2.12.1/dep_cowlib = hex 9.9.9/' $(APP)/Makefile
	$t ! $(MAKE) -C $(APP) --no-print-directory fetch-deps CI_ERLANG_MK= V=0 >$(APP)/download.log 2>&1

	$i "Check that the old archive was not extracted and the lock is unchanged"
	$t test ! -e $(APP)/deps/cowlib
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk
	$t ! grep -q '9.9.9' $(APP)/lock.mk

core-lock-hex-fetch-writes-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowlib 2.12.1 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = hex 2.12.1\n"}' $(APP)/Makefile

	$i "Fetch the dependency"
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v

	$i "Check that the tarball hash is what lock.mk records"
	$t if [ -f $(APP)/.erlang.mk/hex/cowlib.tar ]; then \
		$(call sha256,$(APP)/.erlang.mk/hex/cowlib.tar) | awk '{print $$1}' | tr -d '\n' > $(APP)/cowlib.checksum; \
		test ! -e $(APP)/.erlang.mk/hex/cowlib.tar.checksum; \
	else \
		$(call sha256,$(CACHE_DIR)/hex/cowlib-2.12.1.tar) | awk '{print $$1}' | tr -d '\n' > $(APP)/cowlib.checksum; \
	fi
	$t test `wc -c < $(APP)/cowlib.checksum` -eq 64

	$i "Check that lock.mk records the version and the checksum"
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		'dep_cowlib := hex 2.12.1' \
		'dep_cowlib_commit := 2.12.1' \
		"dep_cowlib_checksum := $$(cat $(APP)/cowlib.checksum)" \
		"dep_hex_core := git $(HEX_CORE_GIT) $$(git -C $(APP)/deps/hex_core rev-parse HEAD)" \
		"dep_hex_core_commit := $$(git -C $(APP)/deps/hex_core rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 2
	$t test `grep -c 'hex.pm' $(APP)/lock.mk` -eq 0
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 0
	$t test `grep -c '^dep_cowlib_checksum := ' $(APP)/lock.mk` -eq 1

	$i "Fetch again and check that the lock appends another Cowlib block"
	$t rm -rf $(APP)/deps/cowlib
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^dep_cowlib := ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_cowlib_checksum := ' $(APP)/lock.mk` -eq 2
	$t test `grep -c '^dep_hex_core := ' $(APP)/lock.mk` -eq 1
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 3

core-lock-hex-locked-fetch-uses-version: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fetch Cowlib 2.12.1 and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = hex 2.12.1\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v
	$t $(MAKE) -C $(APP) lock CI_ERLANG_MK= $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Point the Makefile at 2.13.0 and fetch into an empty directory"
	$t sed -i.bak 's/^dep_cowlib = hex 2.12.1/dep_cowlib = hex 2.13.0/' $(APP)/Makefile
	$t rm -rf $(APP)/deps/cowlib $(APP)/.erlang.mk/hex/cowlib.tar
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v

	$i "Check that the locked version is fetched and the lock is unchanged"
	$t grep -qx 'PROJECT_VERSION = 2.12.1' $(APP)/deps/cowlib/Makefile
	$t ! grep -qx 'PROJECT_VERSION = 2.13.0' $(APP)/deps/cowlib/Makefile
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk
	$t test `grep -c '^dep_cowlib := ' $(APP)/lock.mk` -eq 1
	$t test `grep -c '^dep_hex_core := ' $(APP)/lock.mk` -eq 1

core-lock-hex-locked-makefile-after-include: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fetch Cowlib 2.12.1 and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = hex 2.12.1\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v
	$t $(MAKE) -C $(APP) lock CI_ERLANG_MK= $v

	$i "Override the commit after including Erlang.mk and fetch again"
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before
	$t echo 'dep_cowlib_commit = 2.13.0' >> $(APP)/Makefile
	$t rm -rf $(APP)/deps/cowlib $(APP)/.erlang.mk/hex/cowlib.tar
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v

	$i "Check that an assignment after include erlang.mk leaves the locked version"
	$t grep -qx 'PROJECT_VERSION = 2.12.1' $(APP)/deps/cowlib/Makefile
	$t ! grep -q '2.13.0' $(APP)/lock.mk
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk
	$t test `grep -c '^dep_cowlib := ' $(APP)/lock.mk` -eq 1
	$t test `grep -c '^dep_hex_core := ' $(APP)/lock.mk` -eq 1

	$i "Clear the checksum after including Erlang.mk and fetch again"
	$t echo 'dep_cowlib_checksum =' >> $(APP)/Makefile
	$t rm -rf $(APP)/deps/cowlib $(APP)/.erlang.mk/hex/cowlib.tar
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v

	$i "Check that clearing the checksum after include erlang.mk changes nothing"
	$t grep -qx 'PROJECT_VERSION = 2.12.1' $(APP)/deps/cowlib/Makefile
	$t ! grep -q '2.13.0' $(APP)/lock.mk
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk
	$t test `grep -c '^dep_cowlib := ' $(APP)/lock.mk` -eq 1
	$t test `grep -c '^dep_hex_core := ' $(APP)/lock.mk` -eq 1
core-lock-hex-makefile-checksum: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fetch Cowlib 2.12.1"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowlib\ndep_cowlib = hex 2.12.1\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v
	$t if [ -f $(APP)/.erlang.mk/hex/cowlib.tar ]; then \
		$(call sha256,$(APP)/.erlang.mk/hex/cowlib.tar) | awk '{print $$1}' | tr -d '\n' > $(APP)/cowlib.checksum; \
	else \
		$(call sha256,$(CACHE_DIR)/hex/cowlib-2.12.1.tar) | awk '{print $$1}' | tr -d '\n' > $(APP)/cowlib.checksum; \
	fi

	$i "Set the recorded checksum in the Makefile and fetch again"
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before
	$t perl -ni.bak -e 'if (/^include erlang\.mk/) { print "dep_cowlib_checksum = '"$$(tr -d '\n' < $(APP)/cowlib.checksum)"'\n" } print' $(APP)/Makefile
	$t rm -rf $(APP)/deps/cowlib $(APP)/.erlang.mk/hex/cowlib.tar
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v
	$t grep -qx 'PROJECT_VERSION = 2.12.1' $(APP)/deps/cowlib/Makefile
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk

	$i "Replace the Makefile checksum and check that the fetch is rejected"
	$t sed -i.bak 's/^dep_cowlib_checksum = .*/dep_cowlib_checksum = 0000000000000000000000000000000000000000000000000000000000000000/' $(APP)/Makefile
	$t rm -rf $(APP)/deps/cowlib
	$t ! $(MAKE) -C $(APP) --no-print-directory fetch-deps CI_ERLANG_MK= V=0 >$(APP)/checksum.log 2>&1
	$t grep -q 'Error: checksum mismatch for cowlib.' $(APP)/checksum.log
	$t grep -q 'Error 101' $(APP)/checksum.log
	$t test ! -e $(APP)/deps/cowlib/src
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk
	$t test `grep -c '^ERLANG_MK_LOCK := 1$$' $(APP)/lock.mk` -eq 0

core-lock-hex-package-name: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add uuid 1.8.0 with a package name to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = uuid\ndep_uuid = hex 1.8.0 uuid_erl\n"}' $(APP)/Makefile

	$i "Fetch the dependency"
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v

	$i "Check that the tarball hash is what lock.mk records"
	$t if [ -f $(APP)/.erlang.mk/hex/uuid.tar ]; then \
		$(call sha256,$(APP)/.erlang.mk/hex/uuid.tar) | awk '{print $$1}' | tr -d '\n' > $(APP)/uuid.checksum; \
	else \
		$(call sha256,$(CACHE_DIR)/hex/uuid_erl-1.8.0.tar) | awk '{print $$1}' | tr -d '\n' > $(APP)/uuid.checksum; \
	fi

	$i "Check that lock.mk records the package name, the Git dependency, and not the Hex URL"
	$t test -d $(APP)/deps/uuid
	$t test -d $(APP)/deps/quickrand
	$t test ! -e $(APP)/deps/uuid/lock.mk
	$t test ! -e $(APP)/deps/quickrand/lock.mk
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_hex_core := git $(HEX_CORE_GIT) $$(git -C $(APP)/deps/hex_core rev-parse HEAD)" \
		"dep_hex_core_commit := $$(git -C $(APP)/deps/hex_core rev-parse HEAD)" \
		"dep_quickrand := git https://github.com/okeuday/quickrand.git $$(git -C $(APP)/deps/quickrand rev-parse HEAD)" \
		"dep_quickrand_commit := $$(git -C $(APP)/deps/quickrand rev-parse HEAD)" \
		'dep_uuid := hex 1.8.0 uuid_erl' \
		'dep_uuid_commit := 1.8.0' \
		"dep_uuid_checksum := $$(cat $(APP)/uuid.checksum)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 3
	$t test `git -C $(APP)/deps/quickrand rev-parse HEAD` = `git -C $(APP)/deps/quickrand rev-parse v1.8.0^{commit}`
	$t test `grep -c 'hex.pm' $(APP)/lock.mk` -eq 0
	$t test `grep -c '^dep_uuid := ' $(APP)/lock.mk` -eq 1

core-lock-hex-transitive-fetch-writes-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add Cowboy 2.12.0 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = cowboy\ndep_cowboy = hex 2.12.0\n"}' $(APP)/Makefile

	$i "Fetch the dependency"
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v

	$i "Check that Cowboy and its Git dependencies are recorded in the top-level lock"
	$t test -d $(APP)/deps/cowboy
	$t test -d $(APP)/deps/cowlib
	$t test -d $(APP)/deps/ranch
	$t test ! -e $(APP)/deps/cowboy/lock.mk
	$t test ! -e $(APP)/deps/cowlib/lock.mk
	$t test ! -e $(APP)/deps/ranch/lock.mk
	$t if [ -f $(APP)/.erlang.mk/hex/cowboy.tar ]; then \
		$(call sha256,$(APP)/.erlang.mk/hex/cowboy.tar) | awk '{print $$1}' | tr -d '\n' > $(APP)/cowboy.checksum; \
	else \
		$(call sha256,$(CACHE_DIR)/hex/cowboy-2.12.0.tar) | awk '{print $$1}' | tr -d '\n' > $(APP)/cowboy.checksum; \
	fi
	$t test `wc -c < $(APP)/cowboy.checksum` -eq 64
	$t test `git -C $(APP)/deps/cowlib rev-parse HEAD` = `git -C $(APP)/deps/cowlib rev-parse 2.13.0^{commit}`
	$t test `git -C $(APP)/deps/ranch rev-parse HEAD` = `git -C $(APP)/deps/ranch rev-parse 1.8.0^{commit}`
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		'dep_cowboy := hex 2.12.0' \
		'dep_cowboy_commit := 2.12.0' \
		"dep_cowboy_checksum := $$(cat $(APP)/cowboy.checksum)" \
		"dep_cowlib := git https://github.com/ninenines/cowlib $$(git -C $(APP)/deps/cowlib rev-parse HEAD)" \
		"dep_cowlib_commit := $$(git -C $(APP)/deps/cowlib rev-parse HEAD)" \
		"dep_hex_core := git $(HEX_CORE_GIT) $$(git -C $(APP)/deps/hex_core rev-parse HEAD)" \
		"dep_hex_core_commit := $$(git -C $(APP)/deps/hex_core rev-parse HEAD)" \
		"dep_ranch := git https://github.com/ninenines/ranch $$(git -C $(APP)/deps/ranch rev-parse HEAD)" \
		"dep_ranch_commit := $$(git -C $(APP)/deps/ranch rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 4
	$t test `grep -c 'hex.pm' $(APP)/lock.mk` -eq 0
	$t test `grep -c asciideck $(APP)/lock.mk` -eq 0
	$t test `grep -c 'ci.erlang.mk' $(APP)/lock.mk` -eq 0

core-lock-hex-transitive-hex-fetch-writes-lock: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add idna 6.1.1 to the list of dependencies"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = idna\ndep_idna = hex 6.1.1\n"}' $(APP)/Makefile

	$i "Fetch the dependency"
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v

	$i "Check that the Hex dependency of idna is recorded in the top-level lock"
	$t test -d $(APP)/deps/idna
	$t test -d $(APP)/deps/unicode_util_compat
	$t test ! -e $(APP)/deps/idna/lock.mk
	$t test ! -e $(APP)/deps/unicode_util_compat/lock.mk
	$t for name in idna unicode_util_compat; do \
		if [ -f $(APP)/.erlang.mk/hex/$$name.tar ]; then \
			$(call sha256,$(APP)/.erlang.mk/hex/$$name.tar) | awk '{print $$1}' | tr -d '\n' > $(APP)/$$name.checksum; \
		else \
			tar=`find $(CACHE_DIR)/hex -name "$$name-*.tar" | head -n 1`; \
			test -n "$$tar"; \
			$(call sha256,"$$tar") | awk '{print $$1}' | tr -d '\n' > $(APP)/$$name.checksum; \
		fi; \
		test `wc -c < $(APP)/$$name.checksum` -eq 64; \
	done
	$t printf '%s\n' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_hex_core := git $(HEX_CORE_GIT) $$(git -C $(APP)/deps/hex_core rev-parse HEAD)" \
		"dep_hex_core_commit := $$(git -C $(APP)/deps/hex_core rev-parse HEAD)" \
		'dep_idna := hex 6.1.1' \
		'dep_idna_commit := 6.1.1' \
		"dep_idna_checksum := $$(cat $(APP)/idna.checksum)" \
		'dep_unicode_util_compat := hex 0.7.0 unicode_util_compat' \
		'dep_unicode_util_compat_commit := 0.7.0' \
		"dep_unicode_util_compat_checksum := $$(cat $(APP)/unicode_util_compat.checksum)" \
		'endif' > $(APP)/lock.mk.expected
	$t $(call lock_has_lines,$(APP)/lock.mk.expected)
	$t test `grep -c '^ifdef ERLANG_MK_LOCK$$' $(APP)/lock.mk` -eq 3
	$t grep -q '{vsn, *"0.7.0"' $(APP)/deps/unicode_util_compat/src/unicode_util_compat.app.src
	$t test `grep -c 'hex.pm' $(APP)/lock.mk` -eq 0

core-lock-hex-transitive-hex-locked-fetch-uses-version: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Fetch idna 6.1.1 and lock it"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = idna\ndep_idna = hex 6.1.1\n"}' $(APP)/Makefile
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v
	$t $(MAKE) -C $(APP) lock CI_ERLANG_MK= $v
	$t cp $(APP)/lock.mk $(APP)/lock.mk.before

	$i "Point unicode_util_compat at another version and fetch into an empty directory"
	$t sed -i.bak 's/^dep_unicode_util_compat = hex 0.7.0 /dep_unicode_util_compat = hex 0.7.1 /' $(APP)/deps/idna/Makefile
	$t rm -rf $(APP)/deps/unicode_util_compat
	$t $(MAKE) -C $(APP) fetch-deps CI_ERLANG_MK= $v

	$i "Check that the locked version is fetched and the lock is unchanged"
	$t grep -q '{vsn, *"0.7.0"' $(APP)/deps/unicode_util_compat/src/unicode_util_compat.app.src
	$t ! grep -q '{vsn, *"0.7.1"' $(APP)/deps/unicode_util_compat/src/unicode_util_compat.app.src
	$t cmp $(APP)/lock.mk.before $(APP)/lock.mk
	$t test `grep -c '^dep_unicode_util_compat := ' $(APP)/lock.mk` -eq 1

core-lock-ignore-dep: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Create a Git repository for my_dep"
	$t mkdir $(APP)/git_repo
	$t echo one > $(APP)/git_repo/README
	$t cd $(APP)/git_repo && \
		git init -q -b master && \
		git config user.email "testsuite@erlang.mk" && \
		git config user.name "test suite" && \
		git add README && \
		git commit -q --no-gpg-sign -m "Tests"

	$i "Ignore a cp dependency that is also listed in DEPS"
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep ignored_dep\nIGNORE_DEPS = ignored_dep\ndep_my_dep = git file://$(abspath $(APP)/git_repo) master\ndep_ignored_dep = cp /ignored\n"}' $(APP)/Makefile

	$i "Fetch and lock"
	$t $(MAKE) -C $(APP) fetch-deps $v
	$t $(MAKE) -C $(APP) lock $v

	$i "Check that the ignored dependency was not fetched or required"
	$t test -d $(APP)/deps/my_dep
	$t test ! -e $(APP)/deps/ignored_dep
	$t printf '%s\n' \
		'ERLANG_MK_LOCK := 1' \
		'' \
		'ifdef ERLANG_MK_LOCK' \
		"dep_my_dep := git file://$(abspath $(APP)/git_repo) $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		"dep_my_dep_commit := $$(git -C $(APP)/deps/my_dep rev-parse HEAD)" \
		'endif' > $(APP)/lock.mk.expected
	$t cmp $(APP)/lock.mk.expected $(APP)/lock.mk

core-lock-ln-no-file: init

	$i "Bootstrap a new OTP library named $(APP)"
	$t mkdir $(APP)/
	$t cp ../erlang.mk $(APP)/
	$t $(MAKE) -C $(APP) -f erlang.mk bootstrap-lib $v

	$i "Add a linked dependency"
	$t mkdir $(APP)/my_dep
	$t cp ../erlang.mk $(APP)/my_dep/
	$t $(MAKE) -C $(APP)/my_dep/ -f erlang.mk bootstrap-lib $v
	$t perl -ni.bak -e 'print;if ($$.==1) {print "DEPS = my_dep\ndep_my_dep = ln $(CURDIR)/$(APP)/my_dep/\n"}' $(APP)/Makefile

	$i "Check that make lock leaves the link in place and finds no lock file"
	$t ! $(MAKE) -C $(APP) --no-print-directory lock V=0 >$(APP)/lock.log 2>&1
	$t grep -q 'Error: lock.mk was not found. Fetch dependencies before locking.' $(APP)/lock.log
	$t grep -q 'Error 97' $(APP)/lock.log
	$t test -L $(APP)/deps/my_dep
	$t test ! -e $(APP)/lock.mk
