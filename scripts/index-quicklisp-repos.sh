#!/bin/bash

if [ $# -ne 1 ]
then
    echo 'usage: $0 directory'
    exit 1
fi

ROOT_DIR=${1%/}

rm -rf ../index
mkdir ../index

if [ -f failure.txt ]
then
    rm failure.txt
fi

rm result.txt

temp=$(mktemp)

for f in $(ls -1 $ROOT_DIR)
do
    echo $ROOT_DIR/$f >> $temp
done

parallel -a $temp -j20 ./index-repo.sh
