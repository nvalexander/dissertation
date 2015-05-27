#!/bin/bash
MY_DIR=$(dirname $(readlink -f $0))
ln -f -s ${MY_DIR}/pwl_english.dict  ${HOME}/.lyx/pwl_english.dict
cp /usr/local/share/lyx/ui/stdtoolbars.inc  ~/.lyx/ui/stdtoolbars.inc
#sudo texlua install-getnonfreefonts
#sudo /media/ayf/store/opt/texlive2014/texmf-dist/scripts/getnonfreefonts/getnonfreefonts.pl -a
#sudo /media/ayf/store/opt/texlive2014/texmf-dist/scripts/getnonfreefonts/getnonfreefonts.pl -a
