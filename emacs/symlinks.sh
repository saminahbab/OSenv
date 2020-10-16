#bin/bash


# create symlinks from Github repo to the emacs config dirs
mkdir ~/.emacs.d/lisp

ln -s $(pwd)/init.el ~/.emacs.d/init.el

ln -s $(pwd)/general.el ~/.emacs.d/lisp/general.el
ln -s $(pwd)/packages.el ~/.emacs.d/lisp/packages.el
ln -s $(pwd)/go.el ~/.emacs.d/lisp/go.el
