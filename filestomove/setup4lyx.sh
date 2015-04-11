#!/bin/bash
MY_DIR=$(dirname $(readlink -f $0))
mkdir -p ${HOME}/texmf/tex/latex
ln -f -s ${MY_DIR}/uct10Lyx.clo  ${HOME}/texmf/tex/latex/uct10Lyx.clo
ln -f -s ${MY_DIR}/uct11Lyx.clo  ${HOME}/texmf/tex/latex/uct11Lyx.clo
ln -f -s ${MY_DIR}/uct12Lyx.clo  ${HOME}/texmf/tex/latex/uct12Lyx.clo
ln -f -s ${MY_DIR}/ucthesisLyx.cls  ${HOME}/texmf/tex/latex/ucthesisLyx.cls
mkdir -p ${HOME}/texmf/bibtex/bst
ln -f -s ${MY_DIR}/UCThesisBibStylePlain.bst  ${HOME}/texmf/bibtex/bst/UCThesisBibStylePlain.bst
ln -f -s ${MY_DIR}/UCThesisBibStyleUnsrt.bst  ${HOME}/texmf/bibtex/bst/UCThesisBibStyleUnsrt.bst
mkdir -p ${HOME}/.lyx/layouts
ln -f -s ${MY_DIR}/ucthesisLyx.layout ${HOME}/.lyx/layouts/ucthesisLyx.layout
ln -f -s ${MY_DIR}/biblatex.module ${HOME}/.lyx/layouts/biblatex.module
ln -f -s ${MY_DIR}/pwl_english.dict  ${HOME}/ayf/.lyx/pwl_english.dict
sudo texlua install-getnonfreefonts
sudo /media/ayf/store/opt/texlive2014/texmf-dist/scripts/getnonfreefonts/getnonfreefonts.pl -a
sudo /media/ayf/store/opt/texlive2014/texmf-dist/scripts/getnonfreefonts/getnonfreefonts.pl -a
