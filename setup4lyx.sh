#!/bin/bash
MY_DIR=$(dirname $(readlink -f $0))
ln -f -s ${MY_DIR}/custom.dic ${HOME}/.lyx/pwl_english.dict
ln -f -s ${MY_DIR}/custom.dic ${HOME}/.config/enchant/en_US.dic
ln -f -s ${MY_DIR}/stdtoolbars.inc ${HOME}/.lyx/ui/stdtoolbars.inc
wget http://tug.org/fonts/getnonfreefonts/install-getnonfreefonts
sudo texlua install-getnonfreefonts
sudo /usr/local/bin/getnonfreefonts --sys --all
