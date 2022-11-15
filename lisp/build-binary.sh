#!/usr/bin/env bash

set -euo pipefail

LISP=$1
NAME=$(basename "$1" .lisp)
shift

# :compression 9
sbcl --load "$LISP" \
    --eval "(sb-ext:save-lisp-and-die \"$NAME\"
              :executable t
              :save-runtime-options t
              :toplevel '$NAME:toplevel)"
