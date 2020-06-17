#bin/bash


# create symlinks from Github repo to the emacs config dirs

ln -s $(pwd)/general.el ~/.emacs.d/general.el
ln -s $(pwd)/init.el ~/.emacs.d/init.el
ln -s $(pwd)/packages.el ~/.emacs.d/packages.el
ln -s $(pwd)/go.el ~/.emacs.d/go.el
