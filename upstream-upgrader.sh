#!/bin/sh

set -e

if test $# != 1; then
    echo "Usage: $0 [ocaml version]"
    echo "  e.g. $0 4.13"
    exit 1
fi

CWD=$(dirname "$0")

if test "${CWD}" != '.'; then
    echo "This script can only be called from the root directory of merlin."
    exit 1
fi

case "$1" in
[0-9]\.[0-9][0-9]) ;;
*) echo "Could not parse the version number"; exit 1;;
esac

VERSION=$1
VER=$(echo "${1}" | cut -b1,3,4)

OCAML_SRC=$(mktemp -d)
git clone -b "${VERSION}" git://github.com/ocaml/ocaml.git "${OCAML_SRC}"

rm -rf "upstream/ocaml_${VER}/"
mkdir "upstream/ocaml_${VER}"

cp -r "${OCAML_SRC}/file_formats" "upstream/ocaml_${VER}/"
cp -r "${OCAML_SRC}/parsing" "upstream/ocaml_${VER}/"
cp -r "${OCAML_SRC}/typing" "upstream/ocaml_${VER}/"
cp -r "${OCAML_SRC}/utils" "upstream/ocaml_${VER}/"

rm -f "upstream/ocaml_${VER}"/*/dune
rm -f "upstream/ocaml_${VER}"/*/Makefile
rm -f "upstream/ocaml_${VER}"/*/*.adoc
rm -f "upstream/ocaml_${VER}"/*/*.md
rm -f "upstream/ocaml_${VER}"/*/*.c

git -C "${OCAML_SRC}" show-ref --hash "refs/remotes/origin/${VERSION}" > "upstream/ocaml_${VER}/base-rev.txt"

echo 'Done.'
