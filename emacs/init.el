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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(org-agenda-files '("~/orgs/gtd.org" "~/orgs/journal.org") t)
 '(package-selected-packages
   '(lsp-pyright treemacs-projectile docker-compose-mode yaml-mode elisp-format markdown-preview-mode org-bullets rainbow-mode ripgrep yasnippet-snippets use-package toml-mode tide spacemacs-theme spaceline smex smartparens rjsx-mode rainbow-delimiters prettier-js org-roam magit lsp-ui ivy-rich indium indent-tools go-mode flycheck-rust ein deadgrep cypher-mode counsel-projectile company-lsp cargo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
