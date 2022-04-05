#!/bin/bash

PROGRAM=$0
J=12
N=0
ROOT_DIR=''

usage() {
    echo "usage: $PROGRAM [-j N] [-n N] directory"
}

list_dirs() {
    if [ $N -eq 0 ]
    then
        ls -1 $ROOT_DIR
    else
        ls -1 $ROOT_DIR | head -n $N
    fi
}

while (( $# > 0 ))
do
    case $1 in
        -j)
            shift
            J=$1
            ;;
        -n)
            shift
            N=$1
            ;;
        *)
            ROOT_DIR=${1%/}
            ;;
        esac
        shift
done

if [ -z $ROOT_DIR ]
then
    usage
    exit 1
fi

rm -rf ../index
mkdir ../index

if [ -f failure.txt ]
then
    rm failure.txt
fi

temp=$(mktemp)

for f in $(list_dirs)
do
    echo $ROOT_DIR/$f >> $temp
done

parallel -a $temp -j $J ./index-repo.sh
