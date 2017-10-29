#!/bin/bash

set -eu

DIR=`dirname $0`
cd $DIR

for SOURCE in `ls ../samples`
do
    ../bfc ../samples/${SOURCE}
    NAME=`basename $SOURCE .bf`
    gcc -O2 ${NAME}.c -o a.out
    ./a.out
    rm a.out ${NAME}.c
done
