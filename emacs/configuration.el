(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))
(package-refresh-contents)

(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . "~/.saves")))

(setq find-file-visit-truename t)
(setq backup-directory-alist '(("." . "~/.saves")))
(global-set-key (kbd "C-c m") 'magit)
(setq find-file-visit-truename t)

(set-frame-font "Hack" nil t)

(add-hook 'org-mode-hook 'org-bullets-mode )
(add-hook 'org-mode-hook 'variable-pitch-mode )


(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0
    org-startup-indented t
    org-bullets-bullet-list '(" ")
    org-pretty-entities t
    org-hide-emphasis-markers t
    org-hide-leading-starts t

    org-ellipsis "  " ;; folding symbol
    org-agenda-block-separator ""
    org-fontify-whole-heading-line t
    org-fontify-done-headline t
    org-fontify-quote-and-verse-blocks t
   )

  (setq org-hide-emphasis-markers t)

(lambda () (progn
  (setq left-margin-width 0)
  (setq right-margin-width 0)
  (set-window-buffer nil (current-buffer))))

(setq header-line-format "")

(custom-theme-set-faces 'user '(variable-pitch ((t
                                                 (:family "ETBembo"
                                                          :height 130))))
                        '(fixed-pitch ((t ( :family "Hack"
                                            :height 90
                                            :weight normal )))))

(with-eval-after-load 'org-faces


  (custom-set-faces '(org-document-title ((t :foreground "#1c1e1f"
                                             :underline nil)))
                    '(org-document-info-keyword ((t :foreground "#1c1e1f"
                                                    :underline nil
                                                    :inherit 'fixed-pitch
                                                    :height 70
                                                    :foreground  "#A0A0A0"
                                                    :bold t)))
                    '(org-document-info ((t :foreground "#1c1e1f")))
                    '(org-todo ((t :background nil
                                   :foreground  "#FB6D4C"
                                   :inherit 'fixed-pitch)))
                    '(org-done ((t :background nil
                                   :foreground "#056644"
                                   :inherit 'fixed-pitch)))
                    '(org-link ((t :underline nil
                                   :foreground "#759194")))
                    '(org-level-1 ((t
                                    (:height 1.4
                                             :foreground "#1c1e1f"))))
                    '(org-level-2 ((t
                                    (:height 1.3
                                             :foreground "#1c1e1f"))))
                    '(org-level-3 ((t
                                    (:height 1.2
                                             :foreground "#1c1e1f"))))
                    '(org-level-4 ((t
                                    (:height 1.0
                                             :foreground "#1c1e1f"))))
                    '(org-level-5 ((t
                                    (:height 1.0
                                             :foreground "#1c1e1f"))))
                    '(org-block-begin-line ((t
                                             (:foreground "#787787"
                                                          :background nil
                                                          :inherit 'fixed-pitch
                                                          :bold t
                                                          :height 70))))
                    '(org-block-end-line ((t
                                           (:foreground "#787787"
                                                        :background nil
                                                        :inherit 'fixed-pitch
                                                        :bold t
                                                        :height 70))))

                    '(org-block ((t
                                  (:background nil
                                               :inherit 'fixed-pitch))))
                    '(org-meta-line ((t
                                      (:inherit 'fixed-pitch
                                                :bold t
                                                :height 70
                                                :foreground "#A0A0A0"))))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq-default inhibit-splash-screen t)

(add-hook 'after-init-hook 'global-hl-line-mode)
  (global-visual-line-mode t)
(show-paren-mode 1)

(setq sml/no-confirm-load-theme t)
  (load-theme 'spacemacs-light t )

(use-package
  spaceline
  :after (spaceline-emacs-theme)
  :ensure t)

(setq sml/no-confirm-load-theme t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda()
                              (delete-trailing-whitespace)))

(use-package dashboard
:ensure t
:config
(dashboard-setup-startup-hook))

(use-package toml-mode)

  (use-package rust-mode
    :hook (rust-mode . lsp))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))


(add-hook 'rust-mode-hook #'aggressive-indent-mode)

(setq lsp-rust-server 'rust-analyzer)

(use-package flycheck-rust
    :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook 'company-mode)

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(eval-after-load 'python-mode-hook
  (lambda () (local-set-key (kbd "C-c <tab>") 'python-black-buffer)))

  (use-package blacken
    :ensure t
    :config
    (add-hook 'python-mode-hook 'blacken-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(defun custom-go-hook ()

  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  )

(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)
(setq compilation-window-height 14)
(setq lsp-gopls-codelens nil)

(use-package go-mode :mode "\\*\\.go")
(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook 'custom-go-hook)

(add-hook 'go-mode-hook #'aggressive-indent-mode)

(defun setup-tide-mode()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
(tide-hl-identifier-mode +1)

  (eldoc-mode +1)

  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))

(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)


(use-package web-mode
  :ensure t )

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4))

(use-package rjsx-mode
:ensure t
:mode "\\.js\\'")

(add-hook 'web-mode-hook  'web-mode-init-hook)

(add-hook 'js2-mode-hook #'setup-tide-mode)


(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)

(use-package add-node-modules-path
  :ensure t)
(use-package prettier
  :ensure t
)


(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package cypher-mode
  :ensure t
  :mode "\\.cql")

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
                  :major-modes '(terraform-mode)
                  :server-id 'terraform-ls))


(add-hook 'terraform-mode-hook #'lsp)

(use-package graphql-mode
  :ensure t
  :mode "\\.graphqls?\\'")

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :ensure t)
(add-hook 'yaml-mode-hook #'lsp)

(define-key global-map (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c c r" ) 'comment-region)
(global-set-key (kbd  "C-c c u") 'uncomment-region)
(global-set-key [?\M-h] 'delete-backward-char)
(global-set-key (kbd "<f7>") 'vterm)

(use-package emmet-mode
  :ensure t)

(add-hook 'web-mode-hook 'emmet-mode)

(use-package helm-emmet
  :ensure t)

(use-package forge
  :after magit)

(setq auth-sources '("~/.authinfo"))

(use-package hydra
  :ensure t)

(defhydra hydra-flycheck (global-map "C-c f")
  "Move around flycheck errors"
  ("n" flycheck-next-error "next")
  ("p" flycheck-previous-error "previous")
  ("f" flycheck-first-error "first")
  ("l" flycheck-list-errors "list")
  )

(defhydra hydra-org (global-map "<f1>")
  "Org"
  ("n" org-next-visible-heading "Next Heading")
  ("p" org-previous-visible-heading "Previous Heading")
  ("u" outline-up-heading "Up a level")
  ("," org-promote-subtree "Promote Subtree")
  ("." org-demote-subtree "Demote Subtree")
  ("t" org-todo "TODO")
  ("1" org-priority "Priority")
  )

(use-package
  savehist
  :config (setq history-length 10000))
(savehist-mode)

(use-package pdf-tools
   :pin manual
   :config
   (pdf-tools-install)
   (setq-default pdf-view-display-size 'fit-width)
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(global-set-key (kbd "M-o") 'ace-window)

(use-package smartparens
      :init (sp-use-smartparens-bindings))
    (add-hook 'go-mode-hook #'smartparens-mode)
    (add-hook 'python-mode-hook #'smartparens-mode)
    (add-hook 'rust-mode-hook #'smartparens-mode)

(use-package smartparens-config
  :hook ((after-init . smartparens-global-mode))
  :init (setq sp-hybrid-kill-entire-symbol nil))

(use-package rainbow-delimiters-mode
  :defer t
  :config (add-hook 'prog-mode-hook 'text-mode-hook 'org-src-mode-hook))

  (use-package rainbow-mode
    :ensure t
    :config
    (setq rainbow-x-colors nil)
    (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package
  yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :bind (("<f8>" . yas-expand-from-trigger-key))
  :config (use-package
            yasnippet-snippets
            :ensure t)
  (yas-reload-all))
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("snippets")))

(use-package
company
:ensure t
:init (add-hook 'after-init-hook 'global-company-mode)
:config (setq company-idle-delay 0)
(setq company-minimum-prefix-length 1))

(setq company-tooltip-align-annotations t)

(use-package
  flycheck
  :ensure t
)

(setq
;; helm-bibtex-bibliography '("~/bibtex/bibs.bib")
 bibtex-completion-notes-path "~/orgs/notes/"
 bibtex-completion-bibliography "~/bibtex/bibs.bib"
 bibtex-completion-library-path "~/Zotero/"
 bibtex-completion-pdf-field "file"
 )

(global-set-key (kbd "C-h b") 'helm-bibtex)
(global-set-key (kbd "C-h g") 'helm-do-grep-ag)

(use-package
  counsel
  :config
  ;; Use virtual buffers, this adds bookmarks and recentf to the
  ;; switch-buffer function:
  (setq ivy-use-virtual-buffers t)
  ;; Candidate count format for ivy read. Show index and count.
  (setq ivy-count-format "(%d/%d) ")
  ;; I use big windows, so plenty of room for ivy mini buffer
  (setq max-mini-window-height 0.5)
  (setq ivy-height 20)
  :bind
  ;; Some standard keybindings, matching helm ones I used to have.
  (("M-s o" . 'swiper)
   ("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ;; These keybindings recommended by counsel docs.
   ("<f1> f" . 'counsel-describe-function)
   ("<f1> v" . 'counsel-describe-variable)
   ("<f1> l" . 'counsel-find-library)
   ("<f2> i" . 'counsel-info-lookup-symbol)
   ("<f2> u" . 'counsel-unicode-char)
   ;; use counsel to lookup bookmarks
   ("C-x r l" . 'counsel-bookmark)
   ;; Old keybindings I had from before switching to helm/ivy
   ("<f11>" . nil)
   ("<f11> s" . 'swiper)
   ("<f11> g l" . 'counsel-git-log)
   ("<f11> g b" . 'counsel-git-checkout)
   ("<f11> g a" . 'counsel-ag)
   ("C-c z p f" . 'counsel-projectile-find-file)
   ("C-c z f f" . 'counsel-find-file)
   ("C-c r" . 'ivy-resume)
   ("<f11> u" . 'counsel-unicode-char)))

(use-package
  projectile
  :ensure t
  :config (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))
(use-package
  counsel-projectile
  :bind
  ;; Enable ripgrep with counsel.
  (("C-c g" . #'counsel-projectile-rg)))

(use-package
  counsel-projectile
  :bind
  ;; Enable ripgrep with counsel.
  (("C-c g" . #'counsel-projectile-rg)))

(ivy-mode 1)
(use-package
  ivy-rich
  :init (setq ivy-rich-switch-buffer-name-max-length 100)
  (ivy-rich-mode))

(use-package avy)

(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-#") 'avy-goto-line)

(setq avy-keys
(nconc (number-sequence ?a ?z)
       (number-sequence ?A ?Z)
       (number-sequence ?1 ?9)
       '(?0)))

(use-package smart-shift
  :ensure t)

(global-smart-shift-mode 1)
(setq smart-shift-indentation-level 2)

(use-package highlight-indent-guides
  :ensure t)

(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)

(use-package multiple-cursors
  :ensure t)

(use-package
  lsp-mode
  :ensure t

  :config (add-hook 'python-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'terraform-mode #'lsp))

(setq lsp-sideline-show-hover t)
(setq)

(use-package
  lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init)

(setq lsp-ui-doc-enable t lsp-ui-peek-enable t lsp-ui-sideline-enable t lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t lsp-ui-sideline-toggle-symbols-info t)

(setq lsp-ui-doc-position 'bottom)
(setq lsp-enable-symbol-highlighting t)
(setq lsp-lens-enable t)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-diagnostics-provider :auto)

(setq gc-cons-threshold 200000000)
(setq read-process-output-max ( * 1024 1024) )
(setq lsp-prefer-capf t)

(add-to-list 'lsp-file-watch-ignored "[/\\\\]build$")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]data")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]venv")

(use-package smex)

;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.5 ))

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      Org-agenda-start-day nil ;; i.e. today
      org-agenda-span 1
      org-agenda-start-on-weekday nil)

  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "To refile"
                                   :file-path "orgs/tickler\\.org")
                            (:name "Next to do"
                                   :todo "NEXT"
                                   :order 1)
                            (:name "Important"
                                   :priority "A"
                                   :order 6)
                            (:name "Today's tasks"
                                   :file-path "journal/")
                            (:name "Due Today"
                                   :deadline today
                                   :order 2)
                            (:name "Scheduled Soon"
                                   :scheduled future
                                   :order 8)
                            (:name "Overdue"
                                   :deadline past
                                   :order 7)
                            (:name "Meetings"
                                   :and (:todo "MEET" :scheduled future)
                                   :order 10)
                            (:discard (:not (:todo "TODO")))))))))))

  :config
  (org-super-agenda-mode)
  )
(setq org-agenda-files (list "~/orgs/gtd.org" "~/orgs/code.org" "~/orgs/journal.org"))

(setq org-capture-templates '(("t" "Todo [Inbox]" entry (file+headline "~/orgs/gtd.org" "Tasks")
                                 "* TODO %?\n  %i\n ")
                                ("c" "Code" entry (file+headline "~/orgs/code.org" "Code")
                                 "* TODO %?\n %i\n %a")
                                ("j" "Journal" entry (file+datetree "~/orgs/journal.org")
                                 "* %?\nEntered on %U\n  %i\n  %a")
                                ("T" "Tickler" entry (file+headline "~/orgs/tickler.org" "Tickler")
                                 "* %i%? \n %U")))

(setq org-refile-targets (quote (("~/orgs/tickler.org" :maxlevel . 3)
                                 ("~/orgs/gtd.org" :level . 2)
                                 ("~/orgs/someday.org" :level . 1))))

(define-key global-map (kbd "C-c o")
  (lambda ()
    (interactive)
    (org-capture)))
(define-key global-map (kbd "C-c a")
  (lambda ()
    (interactive)
    (org-agenda)))

(use-package org-roam
      :ensure t
      :hook
      ((after-init . org-roam-mode )
       'org-roam-server-mode)
      :custom
      (org-roam-directory "~/orgs/roam")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show)
               )
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
      )

(setq org-roam-capture-templates
        '(("d" "default" plain
           (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+ROAM_TAGS:\n#+created: %u\n#+last_modified: %U\n\n\n\n"
           :unnarrowed t))

        )

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8081
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(server-start)

(use-package org-roam-bibtex
:requires bibtex-completion
:load-path "~/bibtex/bibs.bib" ;Modify with your own path
:hook (org-roam-mode . org-roam-bibtex-mode)
:bind (:map org-mode-map
            (("C-c n a" . orb-note-actions)))
 )

(add-hook 'after-init-hook #'org-roam-bibtex-mode)

(setq orb-preformat-keywords   '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

  (defvar orb-title-format "${author-or-editor-abbrev} (${date}).  ${title}."
        "Format of the title to use for `orb-templates'.")

(setq orb-templates  `(
      ("r" "ref" plain
      (function org-roam-capture--get-point)
      ""
      :file-name "refs/${citekey}"
      :head ,(s-join "\n"
                     (list
                      (concat "#+title: "
                              orb-title-format)
                      "#+roam_key: ${ref}"
                      "#+created: %U"
                      "#+last_modified: %U\n\n"))
      :unnarrowed t)

     ("n" "ref + noter" plain
      (function org-roam-capture--get-point)
      ""
      :file-name "refs/${citekey}"
      :head ,(s-join "\n"
                     (list
                      "#+title:${title}."
                      "#+ROAM_TAGS:"
                      "#+roam_key: ${ref}"
                      ""
                      "* Notes :noter:"
                      ":PROPERTIES:"
                      ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")"
                      ":NOTER_PAGE:"
                      ":END:\n\n")))))

(use-package org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "~/bibtex/bibs.bib")
         org-ref-bibliography-notes "~/orgs/bibnotes.org"
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "~/orgs/notes/"

    ))

(setq org-roam-capture-ref-templates
      '(("roam" "ref" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "web/${slug}"
         :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_ALIAS:
#+ROAM_TAGS: Link
#+Created: %u
#+LAST_MODIFIED: %U

"
         :unnarrowed t)))

(require 'org-roam-protocol)

(use-package org-noter
  :after (:any org pdf-view)
  :config
(pdf-tools-install)

 (defun zp/org-noter-indirect (arg)
    "Ensure that org-noter starts in an indirect buffer.
Without this wrapper, org-noter creates a direct buffer
restricted to the notes, but this causes problems with the refile
system.  Namely, the notes buffer gets identified as an
agenda-files buffer.
This wrapper addresses it by having org-noter act on an indirect
buffer, thereby propagating the indirectness."
    (interactive "P")
    (if (org-entry-get nil org-noter-property-doc-file)
        (with-selected-window (zp/org-tree-to-indirect-buffer-folded nil t)
          (org-noter arg)
          (kill-buffer))
      (org-noter arg)))


(require 'org-noter-pdftools)
  (setq
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other t
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path "~/orgs/"

   org-noter-auto-save-last-location nil
   )
  :ensure t
  )

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; (use-package org-pdftools
;;   :hook (org-mode . org-pdftools-setup-link))

;; (use-package org-noter-pdftools
;;   :after org-noter
;;   :config
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package deft
    :after org
    :bind
    ("C-c n d" . deft)
    :custom
    (deft-recursive t)
    (deft-use-filter-string-for-filename t)
    (deft-default-extension "org")
    (deft-directory "~/orgs/"))

(require 'org-download)

(use-package org-journal)

(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

(custom-set-variables '(org-trello-files '("~/orgs/am4.trello")))

(use-package jupyter
  :commands (jupyter-run-server-repl
             jupyter-run-repl
             jupyter-server-list-kernels))

(setq org-babel-jupyter-override-src-block "python")
(setq jupyter-eval-use-overlays 1)

(use-package jupyter

  :ensure nil
  :config (progn
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((python . t)
               (jupyter . t)))))

(setq org-publish-project-alist
  '(("html"
     :base-directory "~/org/"
     :base-extension "org"
     :publishing-directory "~/org-exports/"
     :publishing-function org-publish-org-to-html)
    ("pdf"
     :base-directory "~/org/"
     :base-extension "org"
     :publishing-directory "~/org-exports/"
     :publishing-function org-publish-org-to-pdf)
    ("all" :components ("html" "pdf"))))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(
 use-package org-xournalpp
             :ensure t
             :quelpa (org-xournalpp :fetcher gitlab :repo "vherrmann/org-xournalpp" :files ("*.el" "resources"))
             :config
             (add-hook 'org-mode-hook 'org-xournalpp-mode)

 )
