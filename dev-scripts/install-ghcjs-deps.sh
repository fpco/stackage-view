#!/bin/sh

# Run this script from the stackage-view directory, like so:
#
# ./dev-scripts/install-ghcjs-deps.sh

set -x

stack exec -- $PWD/dev-scripts/internal/install-ghcjs-deps.sh
