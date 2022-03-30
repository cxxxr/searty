#!/bin/bash

ROOT_DIR=${1%/}

rm ../index/*
sqlite3 ../index/searty.sqlite3 < ../schema.sqlite3

for dir in $(ls -1 $ROOT_DIR)
do
    repo=$ROOT_DIR/$dir
    ./searty-index.ros $repo
    if [ $? -ne 0 ]
    then
        echo $repo >> failure.txt
    fi
done
