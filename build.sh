#!/bin/sh

# Adapted from
# https://github.com/cicakhq/potato/blob/master/tools/build_binary.sh
pushd src
sbcl --non-interactive \
     --disable-debugger \
     --load main.lisp \
     --eval '(progn (sb-ext:disable-debugger) (sb-ext:save-lisp-and-die "../weeds" :toplevel #'"'"'weeds.main:main :executable t))'
popd
