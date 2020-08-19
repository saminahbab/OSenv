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
    ))




(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)));; install packages from the list that are not yet installed



;; org roam
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/orgs/roam")
      :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-graph-show))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate))))

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


;; smart parents
(use-package smartparens)
(use-package smartparens-config
  :hook ((after-init . smartparens-global-mode))
  :init (setq sp-hybrid-kill-entire-symbol nil))

;; rainbowdelimiters
(use-package rainbow-delimiters
  :defer t
  :hook '(prog-mode-hook text-mode-hook org-src-mode-hook))

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
