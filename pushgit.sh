#!/bin/bash
MY_DIR=$(dirname $(readlink -f $0))
cd ${MY_DIR}
git add .
git commit -m "autoupdate `date +%F-%T`"
git push -u origin master:master
