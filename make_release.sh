#! /usr/bin/env bash
mkdir -p release

VERSION=$1
DOCS="adhd-$VERSION-doc"
STATIC_PATH=$(nix build .#static --no-link --print-out-paths)

cp $STATIC_PATH/bin/adhd ./release/adhd-$VERSION-x86_64-linux
cabal haddock --haddock-all --haddock-option="--odir=release/$DOCS"

cd release

tar caf $DOCS.tar.zst ./$DOCS
