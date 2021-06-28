;; add a capture template.
(add-to-list 'org-capture-templates
	     `("L" "Language Translations" plain (file+headline "~/orgs/languages.org" "Languages")
	       "%i" :immediate-finish t :empty-lines 1 ))

;; set the go translate functions ie -> change the default translations, loop through them.
(setq go-translate-local-language "en")
(setq go-translate-target-language "de")

;; extra for looping.
(setq go-translate-extra-directions '(("en" . "fr") ("en" . "pt")))


;; This is called by the org capture template
(defun capture-translation (toTranslate)
  toTranslate)

;; call this function in the buffer with the highlighted text
(defun translate-and-save (start end)
  (interactive "r")
  (org-capture-string (buffer-substring-no-properties start end) "L")
  )


;; bind this to a good combination
(global-set-key (kbd "<f11>") 'translate-and-save)
