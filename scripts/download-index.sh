#!/bin/bash

base_dir=$1

download_quicklisp_index() {
    curl http://beta.quicklisp.org/dist/quicklisp.txt > "${base_dir}/quicklisp.txt"
}

download_system_index() {
    url=$(grep 'system-index-url: ' "${base_dir}/quicklisp.txt" | cut -d ' ' -f 2)
    curl $url > "${base_dir}/$(basename $url)"
}

download_release_index() {
    url=$(grep 'release-index-url: ' "${base_dir}/quicklisp.txt" | cut -d ' ' -f 2)
    curl $url > "${base_dir}/$(basename $url)"
}

download_quicklisp_index
download_system_index
download_release_index
