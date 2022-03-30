#!/bin/bash

if [ $# -ne 1 ]
then
    echo 'usage: $0 directory'
fi

ROOT_DIR=${1%/}

rm ../index/*
sqlite3 ../index/searty.sqlite3 < ../schema.sqlite3

for dir in $(ls -1 $ROOT_DIR)
do
    repo=$ROOT_DIR/$dir
    ./searty-index $repo
    if [ $? -ne 0 ]
    then
        echo $repo >> failure.txt
    fi
done
