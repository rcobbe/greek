#!/bin/sh
while true ; do
    sleep 15
    cabal clean
    cabal configure --enable-tests
    cabal build
    cabal test
done
