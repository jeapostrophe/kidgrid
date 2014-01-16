#!/bin/zsh
. ~/.zshrc

DEST=~blogs/jeapostrophe.github.com-ng/blog/family

cd ~github/kidgrid

racket -t main.rkt
cp -fr static/* $DEST/kidgrid/

cd $DEST
git add .
git commit -m "kidgrid" . || true
git push
git gc
