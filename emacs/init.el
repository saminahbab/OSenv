(add-to-list 'load-path "~/.emacs.d/lisp")
(load "configuration")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-mode t)
 '(custom-safe-themes
   '("6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "0710b0bdd59c8a7aacf0640591b38fcad5978a0fcfff3fdd999e63499ada8e3e" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "3346f0098a27c74b3e101a7c6b5e57a55cd073a8837b5932bff3d00faa9b76d0" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" default))
 '(ivy-mode t)
 '(org-agenda-files '("~/orgs/gtd.org" "~/orgs/code.org" "~/orgs/journal.org"))
 '(org-roam-bibtex-mode t)
 '(org-roam-server-mode nil)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(org-trello-files '("~/orgs/am4.trello") nil (org-trello))
 '(package-selected-packages
   '(lsp-mode dash sparql-mode lsp-origami origami org-journal-list lsp-tailwindcss lsp-treemacs org-xournalpp quelpa-use-package kubernetes org org-journal imenu-anywhere format-all emojify mood-line bash-completion org-noter org-noter-pdftools org-pdftools org-ref ob org-babel-eval-in-repl jupyter forge magit writeroom-mode mixed-pitch nordless-theme nord-theme vterm curl-to-elisp org-trello ob-ipython blacken helm-emmet emmet-mode enmet-mode prettier prettier-js-mode add-node-modules-path web-mode graphql-mode highlight-indent-guides yasnippet-snippets xterm-color use-package treemacs-projectile transpose-frame toml-mode tide terraform-mode spacemacs-theme spaceline smex smartparens smart-shift rjsx-mode ripgrep rainbow-mode rainbow-delimiters pyvenv python-black prettier-js org-roam-server org-roam-bibtex org-download org-bullets ob-cypher markdown-preview-mode lsp-ui lsp-pyright lsp-docker k8s-mode jq-mode jetbrains-darcula-theme ivy-rich indium indent-tools highlight-indentation go-mode flycheck-yamllint flycheck-rust elisp-format ein doom-themes dockerfile-mode docker-compose-mode deft deadgrep dashboard cython-mode counsel-projectile company-lsp cargo buffer-move aggressive-indent aggressive-fill-paragraph))
 '(request-backend 'url-retrieve))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Hack" :height 90 :weight normal))))
 '(lsp-ui-sideline-code-action ((t (:foreground "light coral"))))
 '(org-block ((t (:background nil :inherit 'fixed-pitch))))
 '(org-block-begin-line ((t (:foreground "#787787" :background nil :inherit 'fixed-pitch :bold t :height 70))))
 '(org-block-end-line ((t (:foreground "#787787" :background nil :inherit 'fixed-pitch :bold t :height 70))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t :foreground "#1c1e1f")))
 '(org-document-info-keyword ((t :foreground "#1c1e1f" :underline nil :inherit 'fixed-pitch :height 70 :foreground "#A0A0A0" :bold t)))
 '(org-document-title ((t :foreground "#1c1e1f" :underline nil)))
 '(org-done ((t :background nil :foreground "#056644" :inherit 'fixed-pitch)))
 '(org-level-1 ((t (:height 1.4 :foreground "#1c1e1f"))))
 '(org-level-2 ((t (:height 1.3 :foreground "#1c1e1f"))))
 '(org-level-3 ((t (:height 1.2 :foreground "#1c1e1f"))))
 '(org-level-4 ((t (:height 1.0 :foreground "#1c1e1f"))))
 '(org-level-5 ((t (:height 1.0 :foreground "#1c1e1f"))))
 '(org-link ((t :underline nil :foreground "#759194")))
 '(org-meta-line ((t (:inherit 'fixed-pitch :bold t :height 70 :foreground "#A0A0A0"))))
 '(org-todo ((t :background nil :foreground "#FB6D4C" :inherit 'fixed-pitch)))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "ETBembo" :height 130)))))
