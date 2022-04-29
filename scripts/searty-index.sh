#!/bin/bash

if [ $# -eq 1 ]; then
    input_file=$(realpath $1)
    output_file=$(realpath "../index/$(basename $input_file .out).db")
elif [ $# -eq 2 ]; then
    input_file=$(realpath $1)
    output_file=$(realpath $2)
else
    echo "usage: $0 input-file output-file"
    exit 1
fi

log_file="$(dirname $output_file)/$(basename $output_file .db).log"
log_dir=$(dirname $output_file)

./searty-index $input_file $output_file 2>&1 | tee $log_file

if [ ${PIPESTATUS[0]} -eq 0 ]; then
    echo $1 >> "${log_dir}/success.txt"
else
    echo $1 >> "${log_dir}/failure.txt"
    rm $output_file
fi
