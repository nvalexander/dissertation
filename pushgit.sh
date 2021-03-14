#!/bin/bash
MY_DIR=$(dirname $(readlink -f $0))
cd ${MY_DIR}
git add .
git commit -m "autoupdate `date +%F-%T`"
<<<<<<< Updated upstream
git push -u origin master
git push -u github master
=======
git push -u origin master:master
>>>>>>> Stashed changes
