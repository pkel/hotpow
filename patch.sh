#!/bin/bash

set -e
shopt -s nullglob

cmd=${1-"apply"}

apply () {
  for patch in ./hotpow_*.patch ; do
    ml=${patch%.patch}.ml
    cp hotpow.ml "$ml"
    patch --no-backup-if-mismatch "$ml" < "$patch"
  done
}

update () {
  for ml in ./hotpow_*.ml ; do
    patch=${ml%.ml}.patch
    diff hotpow.ml "$ml" > "$patch" || echo "updated $patch"
  done
}

clear () {
  for patch in ./hotpow_*.patch ; do
    ml=${patch%.patch}.ml
    rm "$ml" || true
  done
}

case "$cmd" in
  "apply")
    apply
    ;;
  "update")
    update
    ;;
  "clear")
    clear
    ;;
  *)
    echo invalid action
    exit 1
    ;;
esac
