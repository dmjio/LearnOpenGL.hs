#!/usr/bin/env sh
mkdir -p GettingStarted/$1/img
cp -v *.glsl GettingStarted/$1
cp -v Main.hs GettingStarted/$1
cp -v img/* GettingStarted/$1/img
cp -v *.cabal GettingStarted/$1/$1.cabal
sed -i "s/opengl-exp/$1/" GettingStarted/$1/$1.cabal
head -n1 GettingStarted/$1/$1.cabal | awk '{ print $2 }'
# emacs -nw default.nix
touch GettingStarted/$1/LICENSE
tree GettingStarted/$1
