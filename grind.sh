#!/bin/sh
while true ; do
    sleep 60
    cabal clean
    cabal configure --enable-tests
    cabal build
    cabal test
done
