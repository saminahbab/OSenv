#bin/bash


# create symlinks from Github repo to the emacs config dirs
mkdir ~/.emacs.d/lisp

ln -s $(pwd)/init.el ~/.emacs.d/init.el
ln -s $(pwd)/init.org ~/.emacs.d/lisp/init.org
