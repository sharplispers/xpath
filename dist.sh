#!/bin/sh -e

# This shell script prepares web pages, tarballs, and asdf-install symlinks
# for a new Plexippus release.
#
# Documentation is built using atdoc.  We assume that clbuild installed, so
# that "clbuild lisp" will start a Lisp image suitable for the atdoc run.
#
# To minimize network traffic, we copy everything into a directory ~/clnet
# instead copying anything directly to common-lisp.net. 
#
# After running the script, you can sync the results to common-lisp.net like
# this:
#
# rsync -av ~/clnet/project/plexippus-xpath common-lisp.net:/project/

set -x

cd $(dirname $0)
home=$(pwd)
name=$(basename $home)
name_and_date=${name}-$(date --iso)

TMPDIR=`mktemp -d /tmp/dist.XXXXXXXXXX`
cleanup() {
    cd
    rm -rf $TMPDIR
}
trap cleanup exit

make -C doc

darcs tag $name_and_date
darcs put --no-set-default -t $name_and_date $TMPDIR/$name_and_date
rm -rf $TMPDIR/$name_and_date/_darcs

echo '(progn (load "dist.lisp") (quit))' | clbuild lisp 

rsync -a doc $TMPDIR/$name_and_date/

cd $TMPDIR

tgz=$TMPDIR/${name_and_date}.tgz
tar czf $tgz $name_and_date
gpg -b -a $tgz

mkdir -p ~/clnet/project/plexippus-xpath/public_html/

rsync -av \
    $name_and_date/doc/ \
    ~/clnet/project/plexippus-xpath/public_html/

rsync $tgz $tgz.asc ~/clnet/project/plexippus-xpath/public_html/download/

rm -f ~/clnet/project/plexippus-xpath/public_html/download/plexippus-xpath.tar.gz 
rm -f ~/clnet/project/plexippus-xpath/public_html/download/plexippus-xpath.tar.gz.asc

ln -sf ${name_and_date}.tgz ~/clnet/project/plexippus-xpath/public_html/download/plexippus-xpath.tar.gz
ln -sf ${name_and_date}.tgz.asc ~/clnet/project/plexippus-xpath/public_html/download/plexippus-xpath.tar.gz.asc

echo ok
