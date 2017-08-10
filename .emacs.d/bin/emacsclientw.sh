#!/bin/bash

# desc:
#     Allows stdin to be 'piped' to an emacs server.
#
# options:
#     none
#
# usage:
#     $ echo "hello there" | emacsclientw.sh
#     $ cat ~/.emacs | emacsclientw.sh
#     $ emacsclientw.sh ~/.emacs
#
# author:
#     Phil Jackson (phil@shellarchive.co.uk)

# unset DISPLAY

if [ ! -t 0  ]; then

  # TODO merge with our emacs-client function
  # TODO open new window, instead of reusing the same one

    tmp_file="$(mktemp /tmp/emacsclient.XXXXX)"
    lisp_to_accept_file="(progn (fake-stdin-slurp \"$(pwd)\" \"${tmp_file}\") (select-frame-set-input-focus (window-frame (selected-window))))"

    cat > "${tmp_file}"

    emacsclient -a "" -e "${lisp_to_accept_file}" ${@}

    if [ ${?} -ne 0 ]; then
        echo "failed: your input was saved in '${tmp_file}'"
    else
        rm -f "${tmp_file}"
    fi
else
    # nothing from stdin
    emacsclient -n -a "" "${@}"
fi
