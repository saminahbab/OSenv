(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
(package-refresh-contents)

(defvar my-packages
  '(
    ;; General
    deadgrep
    spaceline
    spacemacs-theme
    counsel
    yasnippet
    yasnippet-snippets
    smartparens
    smex
    magit
    projectile
    rainbow-delimiters
    use-package
    ivy-rich
    counsel-projectile
    exec-path-from-shell
    company
    company-lsp
    flycheck
    lsp-mode
    lsp-ui
    indent-tools
    deadgrep
    avy
    helm
    dashboard
    jq-mode

    ;; Python
    ein
    lsp-pyright

    ;; Go
    go-mode

    ;; Typescript/ Javascript
    prettier-js
    tide
    indium

    ;; Rust
    cargo
    rust-mode
    flycheck-rust
    toml-mode

    ;; Neo4j
    cypher-mode

    ;; Org
    org-roam
    org-roam-server
    org-roam-bibtex
    deft
    helm-bibtex
    org-ref
    org-noter
    org-download

    ))



(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)));; install packages from the list that are not yet installed


(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq inhibit-splash-screen t) ;; it'll disable splash screen
;; org roam
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
	   :head "#+title: ${title}\n#+ROAM_TAGS:\n#+created: %u\n#+last_modified: %U\n\n"
	   :unnarrowed t))

	)



;; capture template to grab websites. Requires org-roam protocol.
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
- source :: ${ref}\n\n"
	 :unnarrowed t)))
(require 'org-roam-protocol)

;; and org-roam server
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

;; org ref
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

(defvar orb-title-format "${author-or-editor-abbrev} (${date}).  ${title}."
  "Format of the title to use for `orb-templates'.")

;; Org roam bibtex
(use-package org-roam-bibtex
  :requires bibtex-completion
  :load-path "~/bibtex/bibs.bib" ;Modify with your own path
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
	      (("C-c n a" . orb-note-actions)))
   )

(setq orb-preformat-keywords   '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

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

(setq
 helm-bibtex-bibliography '("~/bibtex/bibs.bib")
 bibtex-completion-notes-path "~/orgs/notes/"
 bibtex-completion-bibliography "~/bibtex/bibs.bib"
 bibtex-completion-library-path "~/Zotero/"
 bibtex-completion-pdf-field "file"
 )

;; pdf tools stuff
(pdf-tools-install)

;; pdfs noter
(use-package org-noter
  :after (:any org pdf-view)
  :config
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

;; pdftools is better than the standard noter thing
;;(use-package org-pdftools
;;  :hook (org-load . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; download pictures
(require 'org-download)
;; DEFT

(use-package deft
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory "~/orgs/"))
;;server start for org roam
(server-start)

;; Typescript Tide Setup
(defun setup-tide-mode()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)

  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; rjsx mode
(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

;;cyopher
(use-package cypher-mode
  :ensure t
  :mode "\\.cql")

;; prettier
;; (use-package prettier-js
;;   :ensure t
;;   :after (rjsx-mode)
;;   :hook (rjsx-mode . prettier-js-mode))

;;
(global-set-key (kbd "M-'") 'kill-buffer-and-window)
;; Avy
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-#") 'avy-goto-line)

(setq avy-keys
      (nconc (number-sequence ?a ?z)
	     (number-sequence ?A ?Z)
	     (number-sequence ?1 ?9)
	     '(?0)))
;; smart parents
(use-package smartparens)
(use-package smartparens-config
  :hook ((after-init . smartparens-global-mode))
  :init (setq sp-hybrid-kill-entire-symbol nil))

;; rainbowdelimiters
(use-package rainbow-delimiters
  :defer t
  :hook '(prog-mode-hook text-mode-hook org-src-mode-hook))
;; why does the above hoook not work?
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; follow symlinked files to origin
(setq find-file-visit-truename t)

;; rust
(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
;; python

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			  (require 'lsp-pyright)
			  (lsp))))  ; or lsp-deferred
;;; terraform
(add-hook 'terraform-mode-hook #'lsp)

;; org journal
(use-package org-journal)
