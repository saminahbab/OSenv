
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

    ;; Python
    ein
    
    ;; Go    
    go-mode

    ;; Typescript/ Javascript
    prettier-js
    tide

    ))


(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)));; install packages from the list that are not yet installed

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

;; prettier
(use-package prettier-js
  :ensure t
  :after (rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))


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
