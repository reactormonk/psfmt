#!/usr/bin/bash

GIT_VERSION=$(git describe --tags)
VERSION=${GIT_VERSION:1}
OS=${uname -s}
DIR=psfmt-$VERSION-$OS
BIN_DIR=release/$DIR/bin
RELEASE_DIR=release/$DIR

stack install --local-bin-path $BIN_DIR --ghc-options '-optl-static -fPIC -Os'
cp README.md $RELEASE_DIR
pushd release
tar --create --file=$DIR.tar.gz $DIR
popd