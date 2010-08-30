#!/bin/bash

# get id and name of the window that has the input

cd $(dirname $0)

WINID=$(./getInputFocus)

function getname {
    xwininfo -id "$1" | grep xwininfo | sed 's/.* id: [^ ]* \(.*\)/\1/'
}


name=$(getname $WINID)

# hack : gtk windows have no name, but the previous window id has the correct one...
if [ "$name" = "(has no name)" ]; then
    name=$(getname $(expr $WINID - 1))
fi

if [ "$name" = "(has no name)" ]; then
    name=""
fi

echo -n $name | sed 's:"\(.*\)":\1:'
