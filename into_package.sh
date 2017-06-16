#!/bin/bash

if [ -d ../rdxmk-0.10/ ]
then
    # Gets a fresh package directory
    rm -rf ../rdxmk-0.10/
    mkdir ../rdxmk-0.10/
else
    mkdir ../rdxmk-0.10/
fi

cp ./rdxmk.el ../rdxmk-0.10/
cp ./rdxmk.info ../rdxmk-0.10/rdxmk.info 
cp ./rdxmk-pkg.el ../rdxmk-0.10/rdxmk-pkg.el
install-info ../rdxmk-0.10/rdxmk.info ../rdxmk-0.10/dir

# Makes a tar archive
pushd ../ > /dev/null
tar -cf rdxmk-0.10.tar rdxmk-0.10/
popd > /dev/null
