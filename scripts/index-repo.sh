#!/bin/bash

repository_path=$1

name=$(basename $repository_path)
root_dir=$(dirname $repository_path)

index_dir=$(realpath ../index/$name)
if [ ! -d $index_dir ]
then
    mkdir $index_dir
fi

sqlite3 $index_dir/searty.db < ../schema.sql

SEARTY_INDEX_DIRECTORY=$index_dir ./searty-index.ros $repository_path 2>&1 > $index_dir/log
status=$?

echo $repository_path $status >> $index_dir/result.txt
