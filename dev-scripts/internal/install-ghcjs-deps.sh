#!/bin/sh

# This is an internal script used by dev-scripts/install-ghcjs-deps.sh.

set -x

cabal --config-file=cabal-ghcjs-config update
cabal --config-file=cabal-ghcjs-config install --ghcjs deps/ghcjs-react/ deps/ghcjs-jquery --force-reinstalls
