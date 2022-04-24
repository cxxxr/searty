#!/bin/bash

if [ $# -ne 3 ]; then
    echo "usage: $0 system-name root-directory output-file"
    exit 1
fi

system_name=$1
root_directory=$2
output_file=$3

log_dir=$(dirname $output_file)
log_file="${log_dir}/$(basename $output_file .out).log"

ros dynamic-space-size=4gb ../lisp-analyzer/lisp-analyzer.lisp $@ 2>&1 | tee $log_file

if [ ${PIPESTATUS[0]} -eq 0 ]; then
    echo $1 >> "${log_dir}/success.txt"
else
    echo $1 >> "${log_dir}/failure.txt"
fi
