#!/bin/bash

base_dir=$1

quicklisp_txt=$(realpath "${base_dir}/quicklisp.txt")
releases_txt=$(realpath "${base_dir}/releases.txt")

version=$(grep 'version:' $quicklisp_txt | cut -d ' ' -f 2)

release_dir=$(realpath "${base_dir}/${version}")
rm -rf $release_dir
mkdir -p $release_dir

while read line
do
    echo $line | grep '^#' > /dev/null
    if [ $? -eq 1 ]
    then
        project_name=$(echo $line | cut -d ' ' -f 1)
        url=$(echo $line | cut -d ' ' -f 2)
        echo $project_name $url
        curl $url -o "${release_dir}/${project_name}.tgz"
    fi
done < $releases_txt

cd $release_dir

for file in $(ls -1 *.tgz)
do
    tar -xf $file
done

rm *.tgz
