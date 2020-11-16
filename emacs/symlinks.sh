#bin/bash


# create symlinks from Github repo to the emacs config dirs
mkdir ~/.emacs.d/lisp

ln -s $(pwd)/init.el ~/.emacs.d/init.el
ln -s $(pwd)/configuration.el ~/.emacs.d/lisp/configuration.el
