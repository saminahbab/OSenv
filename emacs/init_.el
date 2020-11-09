;; (setenv "HOME" "/Users/saminahbab")
(add-to-list 'load-path "~/.emacs.d/lisp")


(require 'package)

(load "packages")
(load "general")


(when (not package-archive-contents)
  (package-refresh-cpntents))


(setq lsp-rust-server 'rust-analyzer)

;; Theming
(load-theme 'jetbrains-darcula t)
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
   '("d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "3346f0098a27c74b3e101a7c6b5e57a55cd073a8837b5932bff3d00faa9b76d0" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(org-agenda-files
   '("~/OSenv/emacs/configuration.org" "~/OSenv/terminal/configuration.org" "~/orgs/roam/web/finally_we_may_have_a_path_to_the_fundamental_theory_of_physics_and_it_s_beautiful_stephen_wolfram_writings.org" "~/orgs/gtd.org" "~/orgs/code.org" "~/orgs/journal.org"))
 '(org-roam-server-mode t)
 '(package-selected-packages
   '(transpose-frame buffer-move org-journal smart-shift highlight-indentation k8s-mode dockerfile-mode lsp-docker xterm-color flycheck-yamllint terraform-mode jetbrains-darcula-theme cython-mode org-noter-pdftools org-download yasnippet-snippets use-package treemacs-projectile toml-mode tide spacemacs-theme spaceline smex smartparens rjsx-mode ripgrep rainbow-mode rainbow-delimiters pyvenv python-black prettier-js org-roam-server org-roam-bibtex org-ref org-noter org-bullets ob-cypher markdown-preview-mode magit lsp-ui lsp-pyright ivy-rich indium indent-tools go-mode flycheck-rust elisp-format ein doom-themes docker-compose-mode deft deadgrep counsel-projectile company-lsp cargo aggressive-indent aggressive-fill-paragraph)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
