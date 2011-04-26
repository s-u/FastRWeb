#!/bin/sh

## this is a script that can be used to create the
## sample setup of FastRWeb on a unix server in the
## default location of /var/FastRWeb

ROOT=/var/FastRWeb

PWD=`pwd`
if [ ! -e "$PWD/install.sh" -o ! -e "$PWD/web.R/common.R" ]; then
    echo ''
    echo " This script must be run from the directory of the installed"
    echo " FastRWeb package! Please use something like:"
    echo ''
    echo 'cd `echo '"'"'cat(system.file(package="FastRWeb"))'"'"' | R --slave`'
    echo 'sh install.sh'
    exit 1
fi

mkdir "$ROOT" 2>/dev/null
if ! touch "$ROOT/foo"; then
    echo ''
    echo "ERROR: unable to write in $ROOT"
    echo "       you may need to run this as root, e.g.: sudo sh install.sh"
    exit 1
fi

rm -f "$ROOT/foo"
for dir in code tmp web web.R; do
    cp -pR "$dir" "$ROOT/$dir"
done

echo "Done."
echo "Please check files in $ROOT/code"
echo "If they match tour setup, you can start Rserve using"
echo "$ROOT/code/start"
