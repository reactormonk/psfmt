#!/usr/bin/bash

git describe --tags --exact-match || exit 1

GIT_VERSION=$(git describe --tags --exact-match)
VERSION=${GIT_VERSION:1}
OS=linux
DIR=psfmt-$VERSION-$OS
BIN_DIR=release/$DIR/bin
RELEASE_DIR=release/$DIR

stack install --local-bin-path $BIN_DIR --ghc-options '-optl-static -fPIC -Os'
cp README.md $RELEASE_DIR
pushd release
tar --create --file=$DIR.tar.gz $DIR
popd