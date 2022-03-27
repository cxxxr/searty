#!/bin/bash

if [ $# -eq 1 ]
then
    working_dir=$1
else
    working_dir=$(pwd)
fi

VERSION=$(curl http://beta.quicklisp.org/dist/quicklisp.txt | grep 'canonical-distinfo-url: ' | sed 's/canonical-distinfo-url:\s*//' | cut -d '/' -f 6)

release_dir="${working_dir}/releases/${VERSION}"
mkdir -p $release_dir
cd $release_dir

curl "http://beta.quicklisp.org/dist/quicklisp/${VERSION}/releases.txt" -o releases.txt

for url in $(grep -v '^#' releases.txt | cut -d ' ' -f 2)
do
    name=$(echo $url | sed 's@http://beta.quicklisp.org/archive/\([^/]*\)/.*\.tgz@\1@')
    file=$(echo $url | sed 's@http://beta.quicklisp.org/archive/\([^/]*\)/\(.*\.tgz\)@\2@')
    curl $url -o "${release_dir}/${name}.tgz"
done

cd $release_dir

for file in $(ls -1 *.tgz)
do
    tar -xf $file
done

rm *.tgz
