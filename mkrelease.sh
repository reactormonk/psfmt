#!/usr/bin/bash

git describe --tags --exact-match || exit 1

GIT_VERSION=$(git describe --tags --exact-match)
VERSION=${GIT_VERSION:1}
OS=linux

DIR=psfmt-$VERSION-$OS
BIN_DIR=$DIR/bin
mkdir -p BIN_DIR
stack install --local-bin-path $BIN_DIR
tar cf $BIN_DIR