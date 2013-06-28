#!/bin/bash

# make some magic

make(){
    pushd build/4.11

    # unpack the downloaded projects
    # for file in *.tar*; do
    #     if [ ! -d $file ]; then
    #         echo "unpacking $file"
    #         tar -xf $file
    #     fi
    # done

    # patch required projects
    declare -a needs_patch=(binutils gcc newlib)
    for project in "${needs_patch[@]}"; do
        magic=`find . -type d -name "*$project*-"`
        echo "entering $magic"
        pushd $magic
        popd
    done
}
make
