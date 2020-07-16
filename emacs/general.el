
(setq-default custom-file "~/.gnu-emacs-custom")

;; Don't use lock files (which also clutter up my view of a directory.
(setq create-lockfiles nil)

;;save directory is away from project
(setq backup-directory-alist '(("." . "~/.saves")))

;; highlight line that you are on
(show-paren-mode 1)

;; yes-or-no
;; enable quick confirm with y or n instead of yes or no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; theming ? 
(use-package spaceline
  :after (spaceline-emacs-theme)
  :ensure t )


;; maximise real estate and get rid of splash
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(setq-default inhibit-splash-screen t)

(use-package savehist
  :config 
    (setq history-length 10000))
(savehist-mode)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))


(use-package counsel
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
     ("C-c o p f" . 'counsel-projectile-find-file)
     ("C-c o f f" . 'counsel-find-file)
     ("C-c r" . 'ivy-resume)
     ("<f11> u" . 'counsel-unicode-char)))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
    (projectile-mode +1))

(use-package counsel-projectile
  :bind
     ;; Enable ripgrep with counsel.
  (("C-c f" . #'counsel-projectile-rg)))
(use-package smex)

(ivy-mode 1)
(use-package ivy-rich
  :init
  (setq ivy-rich-switch-buffer-name-max-length 100)
  (ivy-rich-mode))
(use-package smartparens
  :init
(sp-use-smartparens-bindings))
(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)

(use-package flycheck :ensure t
  :bind
  (
   ("C-c l p" . 'flycheck-previous-error)
   ("C-c l n" . 'flycheck-next-error)
   ))

;; LSP

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  )


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  )

(setq lsp-ui-doc-enable t
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t
      lsp-ui-sideline-toggle-symbols-info t)

;; yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (use-package yasnippet-snippets :ensure t )
  (yas-reload-all))

;; ;; yasnippet conflicts between company and Yasnippet
;;   (defun check-expansion ()
;;     (save-excursion
;;       (if (looking-at "\\_>") t
;;         (backward-char 1)
;;         (if (looking-at "\\.") t
;;           (backward-char 1)
;;           (if (looking-at "->") t nil)))))

;;   (defun do-yas-expand ()
;;     (let ((yas/fallback-behavior 'return-nil))
;;       (yas/expand)))

;;   (defun tab-indent-or-complete ()
;;     (interactive)
;;     (if (minibufferp)
;;         (minibuffer-complete)
;;       (if (or (not yas/minor-mode)
;;               (null (do-yas-expand)))
;;           (if (check-expansion)
;;               (company-complete-common)
;;             (indent-for-tab-command)))))

<<<<<<< variant A
;;   (global-set-key [tab] 'tab-indent-or-complete)
>>>>>>> variant B
  ;; (global-set-key [tab] 'tab-indent-or-complete)
======= end

;; terminal
(setq-default
 shell-file-name "/bin/zsh")


;; lsp tuning
(setq gc-cons-threshold 200000000)
(setq read-process-output-max ( * 1024 1024) )
(setq lsp-prefer-capf t)

(add-to-list 'lsp-file-watch-ignored "[/\\\\]build$")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]data")


