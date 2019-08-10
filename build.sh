#!/bin/sh

# https://github.com/cicakhq/potato/blob/master/tools/build_binary.sh

#  The dumped binary calls SERVER-CONFIG2:LAUNCH-SERVICE which
#  processes the configuration file, startup options and allows the
#  user to choose which service to start.

sbcl --non-interactive \
     --disable-debugger \
     --load src/util.lisp \
     --load src/tree.lisp \
     --load src/main.lisp \
     --eval '(progn (sb-ext:disable-debugger) (sb-ext:save-lisp-and-die "cl-blog" :toplevel #'"'"'main :executable t))'
