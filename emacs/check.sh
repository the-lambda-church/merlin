#!/bin/sh -e

# Adapted from https://github.com/purcell/package-lint/blob/master/run-tests.sh
EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="package-lint company iedit auto-complete"

TO_CHECK=*.el

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (add-to-list 'package-archives \
    '(\"melpa\" . \"https://melpa.org/packages/\") t) \
  (package-initialize) \
  (package-refresh-contents) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Refresh package archives, because the test suite needs to see at least
# package-lint and cl-lib.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

"$EMACS" -Q -batch \
         -L . \
         --eval "$INIT_PACKAGE_EL" \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         ${TO_CHECK}

# Lint failures are ignored if EMACS_LINT_IGNORE is defined, so that lint
# failures on Emacs 24.2 and below don't cause the tests to fail, as these
# versions have buggy imenu that reports (defvar foo) as a definition of foo.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -L . \
         --eval "(require 'package-lint)" \
         -f package-lint-batch-and-exit \
         ${TO_CHECK} || [ -n "${EMACS_LINT_IGNORE+x}" ]
