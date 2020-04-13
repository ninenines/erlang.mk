#!/bin/sh

if [ -z "$1$2$3$4$5$6" ]
then
	echo "Missing arguments" 1>&2
	exit 1
fi

T=`echo "$1" | grep "\." | wc -l `
if [ "$T" -eq "1" ]
then
	echo "Invalid dot character in name. Considere replace by an underscore." 1>&2
	exit 1
fi

PKG_FILE="index/$1.mk"
CONTENTS="PACKAGES += $1
pkg_$1_name = $1
pkg_$1_description = $6
pkg_$1_homepage = $5
pkg_$1_fetch = $2
pkg_$1_repo = $3
pkg_$1_commit = $4
"
COMMIT="Add package $1 to the index"

printf "$CONTENTS" > $PKG_FILE
git add $PKG_FILE
git commit -m "$COMMIT" $PKG_FILE
