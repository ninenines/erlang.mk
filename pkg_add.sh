#!/bin/sh

PKG_FILE="packages.v2.tsv"
LINE="$1\t$2\t$3\t$4\t$5\t$6\n"
COMMIT="Add package $1 to the index"

printf "$LINE" >> $PKG_FILE
sort $PKG_FILE -o $PKG_FILE
make
git commit -m "$COMMIT" packages.*
