#!/bin/bash

BOOTSTRAP_VERSION=$1

if [ -z "$BOOTSTRAP_VERSION" ]; then
    echo "You must specificy the bootstrap version."
    exit 1
fi

set -ex

SOURCES='http://common-lisp.net/project/lispy/repository/distfiles'

# (mapcar #'(lambda (module) 
# 	    (let ((version (lispy:latest-version module)))
# 	      (write-line (lispy:source version))))
# 	(sort (remove-duplicates (cons (lispy:module-by-name :asdf)
# 				       (lispy::dependency-list (lispy:module-by-name :lispy))))
# 	      #'string<
# 	      :key #'lispy:name))

DEPENDENCIES='
alexandria-2009-08-13.tar.gz
archive_0.7.0.tar.gz
asdf-1.108.tar.gz
babel_0.3.0.tar.gz
cffi_0.10.5.tar.gz
chunga-1.1.0.tar.gz
cl+ssl-2008-11-04.tar.gz
cl-base64-3.3.3.tar.gz
cl-fad-0.6.2.tar.gz
cl-ppcre-2.0.1.tar.gz
drakma-1.0.0.tar.gz
flexi-streams-1.0.7.tar.gz
gzip-stream_0.1.tar.gz
ironclad_0.27.tar.gz
lispy-0.5.tar.gz
lispy-gpgme.tar.gz
log5_0.3.1.tar.gz
puri-1.5.1.tar.gz
salza-0.7.4.tar.gz
split-sequence-2002-04-10.tar.gz
trivial-features_0.5.tar.gz
trivial-gray-streams-2008-11-02.tar.gz
trivial-shell_2008-09-01.tar.gz
usocket-0.4.1.tar.gz'

TMP=`mktemp -d -t bootstrap.sh.XXXXXX`
# trap "rm -rf $TMP* 2>/dev/null" EXIT

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
    echo "Found $path"
    cat >>$TMP/lispy-all/asdf-config.lisp <<EOF
         #p"${path}/"
EOF
done
cat >>$TMP/lispy-all/asdf-config.lisp <<EOF
        ))
    (PUSHNEW (MERGE-PATHNAMES PATH ROOT) ASDF:*CENTRAL-REGISTRY* :TEST 'EQUAL))) 
EOF
cat $TMP/lispy-all/asdf-config.lisp

cp $TMP/lispy-all/asdf-*/asdf.lisp $TMP/lispy-all/
cat >>$TMP/load.lisp <<EOF
(require :asdf)
(load "$TMP/lispy-all/asdf-config.lisp")
(asdf:oos 'asdf:load-op :lispy)
(lispy:install (lispy:module-by-name :lispy))
(quit)
EOF

sbcl --no-userinit --no-sysinit --load $TMP/load.lisp

cp -a $TMP/lispy-all/ $TMP/lispy-all-$BOOTSTRAP_VERSION
find $TMP/lispy-all-$BOOTSTRAP_VERSION -type f -name \*.fasl -print0 |xargs -0 rm -f
rm -rf $TMP/lispy-all-$BOOTSTRAP_VERSION/distfiles/*
tar cfz ~/lispy-all-$BOOTSTRAP_VERSION.tar.gz -C $TMP lispy-all-$BOOTSTRAP_VERSION

