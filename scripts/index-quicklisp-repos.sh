#!/bin/bash

if [ $# -ne 1 ]
then
    echo 'usage: $0 directory'
    exit 1
fi

ROOT_DIR=${1%/}

find ../index -type f | xargs rm
sqlite3 ../index/searty.db < ../schema.sql

for dir in $(ls -1 $ROOT_DIR)
do
    repo=$ROOT_DIR/$dir
    echo '---' $repo
    ./searty-index $repo
    echo
    if [ $? -ne 0 ]
    then
        echo $repo >> failure.txt
    fi
done
