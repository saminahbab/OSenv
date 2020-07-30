;; (setenv "HOME" "/Users/saminahbab")
(add-to-list 'load-path "~/.emacs.d")

(require 'package)


(load "packages")
(load "general")


(when (not package-archive-contents)
  (package-refresh-cpntents))

;; Theming
(load-theme 'spacemacs-light)
(use-package spaceline)
(spaceline-emacs-theme)

;; languages
(load "go")
