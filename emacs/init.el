(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'org)


(org-babel-load-file (expand-file-name (concat user-emacs-directory "lisp/init.org")))
