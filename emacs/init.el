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
 '(org-agenda-files
   '("~/orgs/roam/20201112204422-database_issues.org" "~/orgs/gtd.org" "~/orgs/code.org" "~/orgs/journal.org"))
 '(org-roam-bibtex-mode t)
 '(org-roam-server-mode nil)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   '(writeroom-mode org-pdftools mixed-pitch nordless-theme nord-theme vterm forge curl-to-elisp org-trello ob-ipython pdf-view-restore blacken helm-emmet emmet-mode enmet-mode prettier prettier-js-mode add-node-modules-path web-mode graphql-mode org-superstar org-super-agenda highlight-indent-guides yasnippet-snippets xterm-color use-package treemacs-projectile transpose-frame toml-mode tide terraform-mode spacemacs-theme spaceline smex smartparens smart-shift rjsx-mode ripgrep rainbow-mode rainbow-delimiters pyvenv python-black prettier-js org-roam-server org-roam-bibtex org-ref org-noter-pdftools org-journal org-download org-bullets ob-cypher markdown-preview-mode magit lsp-ui lsp-pyright lsp-docker k8s-mode jq-mode jetbrains-darcula-theme ivy-rich indium indent-tools highlight-indentation go-mode flycheck-yamllint flycheck-rust elisp-format ein doom-themes dockerfile-mode docker-compose-mode deft deadgrep dashboard cython-mode counsel-projectile company-lsp cargo buffer-move aggressive-indent aggressive-fill-paragraph))
 '(request-backend 'url-retrieve))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background nil))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
