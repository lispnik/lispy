#!/bin/bash

set -x

SOURCES='http://common-lisp.net/project/lispy/repository/distfiles'

# (mapcar #'(lambda (module) 
#             (let ((version (lispy:latest-version module)))
#               (write-line (lispy:source version))))
#         (sort (remove-duplicates(lispy::dependency-list (lispy:module-by-name :lispy)))
#               #'string<
#               :key #'lispy:name))

DEPENDENCIES='
archive_0.6.0-lispy.tar.gz
cffi-070620-lispy.tar.gz
chunga-0.3.0-lispy.tar.gz
cl+ssl-2007-07-07-lispy.tar.gz
cl-base64-3.3.2-lispy.tar.gz
cl-fad-0.6.0-lispy.tar.gz
drakma-0.9.1-lispy.tar.gz
flexi-streams-0.11.2-lispy.tar.gz
gzip-stream_0.1-lispy.tar.gz
ironclad_0.22-lispy.tar.gz
lispy-0.3.tar.gz
puri-1.5.1-lispy.tar.gz
salza-0.7.4-lispy.tar.gz
split-sequence-2002-04-10-lispy.tar.gz
trivial-gray-streams-2006-09-16-lispy.tar.gz
usocket-0.3.3-lispy.tar.gz'

TMP=`mktemp -d -t bootstrap.sh.XXXXXX`
trap "rm -rf $TMP* 2>/dev/null" EXIT

mkdir -p $TMP/lispy-all/distfiles

for d in $DEPENDENCIES; do 
    wget -q -P $TMP/lispy-all/distfiles -nd "$SOURCES/$d"
    tar xfz "$TMP/lispy-all/distfiles/$d" -C $TMP/lispy-all
done

cat >$TMP/lispy-all/asdf-config.lisp <<EOF
(LET ((ROOT (MAKE-PATHNAME :DIRECTORY (PATHNAME-DIRECTORY *LOAD-TRUENAME*))))
  (DOLIST
      (PATH
       '(
EOF
find $TMP/lispy-all -mindepth 1 -maxdepth 1 -type d -not -name distfiles -print |sed -e "s,$TMP/lispy-all/,," | while read path; do
    cat >>$TMP/lispy-all/asdf-config.lisp <<EOF
         #p"${path}/"
EOF
done
cat >>$TMP/lispy-all/asdf-config.lisp <<EOF
        ))
    (PUSHNEW (MERGE-PATHNAMES PATH ROOT) ASDF:*CENTRAL-REGISTRY* :TEST 'EQUAL))) 
EOF
cat $TMP/lispy-all/asdf-config.lisp

cat >>$TMP/load.lisp <<EOF
(require :asdf)
(load "$TMP/lispy-all/asdf-config.lisp")
(asdf:oos 'asdf:load-op :lispy)
(lispy:install (lispy:module-by-name :lispy))
(quit)
EOF

sbcl --no-userinit --no-sysinit --load $TMP/load.lisp

DATE=`date -I`
cp -a $TMP/lispy-all/ $TMP/lispy-all-$DATE
find $TMP/lispy-all-$DATE -type f -name \*.fasl -print0 |xargs -0 rm -f
rm -rf $TMP/lispy-all-$DATE/distfiles/*
tar cfz ~/lispy-all-$DATE.tar.gz -C $TMP lispy-all-$DATE
