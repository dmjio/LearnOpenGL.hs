#!/usr/bin/env sh

mkdir -p GettingStarted/$1/img
cp -v *.glsl GettingStarted/$1
cp -v Main.hs GettingStarted/$1
cp -v img/* GettingStarted/$1/img
touch GettingStarted/$1/LICENSE
tree GettingStarted/$1
