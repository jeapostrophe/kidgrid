#!/bin/zsh
. ~/.zshrc

DEST=~blogs/jeapostrophe.github.com-ng/blog/family

cd ~github/kidgrid

make
cp -fr schedule.pdf $DEST/kidgrid/

cd $DEST
git add .
git commit -m "kidgrid" . || true
git push
git gc
