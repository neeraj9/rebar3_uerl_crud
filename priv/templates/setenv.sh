#!/bin/sh
#
# use it as follows
#   source setenv.sh
#
# (c) copyright 2016 Neeraj Sharma

MYDIR="$(pwd)"


# set environment for Erlang
export PATH=${MYDIR}:${MYDIR}/build/rumprun-packages/erlang/build/host_erlangdist/opt/erlang/lib/erlang/erts-7.3.1/bin:$PATH

# set environment for elixir bin (hex, mix, etc)
#export PATH=${MYDIR}/build/elixir/bin:$PATH
