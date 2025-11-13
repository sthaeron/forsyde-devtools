#! /bin/bash
. ~/.nix-profile/etc/profile.d/nix.sh
nix develop --command bash -c "cabal exec forsyde-lsp-exe --" &
sleep 2 # Add a dumb delay because nix develop is so slow
./klighd-linux --ls_port 5007 $1
