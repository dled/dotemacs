;;; elisp/init-web.el
(use-package web-mode
   :ensure t)

(use-package mustache-mode
   :ensure t)

;; emmet
(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)  ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

;; skewer
(use-package skewer-mode
  :ensure t
  :commands skewer-mode run-skewer
  :config (skewer-setup))


;; color ident minor mode
(use-package color-identifiers-mode
  :ensure t
  :init
    (add-hook 'emacs-lisp-mode-hook 'color-identifiers-mode)
  :diminish color-identifiers-mode)

;; insert comment with eval
(use-package lisp-mode
  :config (defun eval-and-comment-output ()
            "Add the output of the sexp as a comment after the sexp"
            (interactive)
            (save-excursion
              (end-of-line)
              (condition-case nil
                  (princ (concat " ; -> " (pp-to-string (eval (preceding-sexp))))
                         (current-buffer))
                (error (message "Invalid expression")))))

  :bind ("C-x e" . eval-and-comment-output))

;; block wrappers
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-<") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-`") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

;; wrap regions
;; https://github.com/rejeep/wrap-region.el
(use-package wrap-region
  :ensure   t
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
   '(("(" ")")
     ("[" "]")
     ("{" "}")
     ("<" ">")
     ("'" "'")
     ("\"" "\"")
     ("‘" "’"   "q")
     ("“" "”"   "Q")
     ("*" "*"   "b"   org-mode)                 ; bolden
     ("*" "*"   "*"   org-mode)                 ; bolden
     ("/" "/"   "i"   org-mode)                 ; italics
     ("/" "/"   "/"   org-mode)                 ; italics
     ("~" "~"   "c"   org-mode)                 ; code
     ("~" "~"   "~"   org-mode)                 ; code
     ("=" "="   "v"   org-mode)                 ; verbatim
     ("=" "="   "="   org-mode)                 ; verbatim
     ("_" "_"   "u" '(org-mode markdown-mode))  ; underline
     ("**" "**" "b"   markdown-mode)            ; bolden
     ("*" "*"   "i"   markdown-mode)            ; italics
     ("`" "`"   "c" '(markdown-mode ruby-mode)) ; code
     ("`" "'"   "c"   lisp-mode)                ; code
     ))
  :diminish wrap-region-mode)

;; surround text
(defun surround (start end txt)
  "Wraps the specified region (or the current 'symbol / word'
with some textual markers that this function requests from the
user. Opening-type text, like parens and angle-brackets will
insert the matching closing symbol.

This function also supports some org-mode wrappers:

  - `#s` wraps the region in a source code block
  - `#e` wraps it in an example block
  - `#q` wraps it in an quote block"
  (interactive "r\nsEnter text to surround: " start end txt)

  ;; If the region is not active, we use the 'thing-at-point' function
  ;; to get a "symbol" (often a variable or a single word in text),
  ;; and use that as our region.

  (if (not (region-active-p))
      (let ((new-region (bounds-of-thing-at-point 'symbol)))
        (setq start (car new-region))
        (setq end (cdr new-region))))

  ;; We create a table of "odd balls" where the front and the end are
  ;; not the same string.
  (let* ((s-table '(("#e" . ("#+BEGIN_EXAMPLE\n" "\n#+END_EXAMPLE") )
                    ("#s" . ("#+BEGIN_SRC \n"    "\n#+END_SRC") )
                    ("#q" . ("#+BEGIN_QUOTE\n"   "\n#+END_QUOTE"))
                    ("<"  . ("<" ">"))
                    ("("  . ("(" ")"))
                    ("{"  . ("{" "}"))
                    ("["  . ("[" "]"))))    ; Why yes, we'll add more
         (s-pair (assoc-default txt s-table)))

    ;; If txt doesn't match a table entry, then the pair will just be
    ;; the text for both the front and the back...
    (unless s-pair
      (setq s-pair (list txt txt)))

    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert (car s-pair))
      (goto-char (point-max))
      (insert (cadr s-pair))
      (widen))))

(global-set-key (kbd "C-+") 'surround)

;; wrap wrapper ..?
(defun surround-text (txt)
  (if (region-active-p)
      (surround (region-beginning) (region-end) txt)
    (surround nil nil txt)))

;; interactive lambda expr to keybind
(defun surround-text-with (surr-str)
  "Returns an interactive function that when called, will surround the region (or word) with the SURR-STR string."
  (lexical-let ((text surr-str))
    (lambda ()
      (interactive)
      (surround-text text))))

(defun surround-html (start end tag)
   "Wraps the specified region (or the current 'symbol / word'
 with a properly formatted HTML tag."
   (interactive "r\nsTag: " start end tag)
   (save-excursion
     (narrow-to-region start end)
     (goto-char (point-min))
     (insert (format "<%s>" tag))
     (goto-char (point-max))
     (insert (format "</%s>" tag))
     (widen)))

;;(define-key html-mode-map (kbd "C-c C-w") 'surround-html)

(provide 'init-web)

;; init-web.el ends here
