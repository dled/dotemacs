;;; elisp/init-clojure.el

;;(use-package clojure-mode
;;  :ensure t
;;  :init
;;  ((defconst clojure--prettify-symbols-alist
;;     '(("fn"   . ?λ)
;;       {"__"   . ?⁈)))
;;  :config
;;  (add-hook 'clojure-mode-hook 'global-prettify-symbols-mode))

(use-package clojure-mode
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package color-identifiers-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'color-identifiers-mode))

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in)

  :init
  (setq cider-auto-select-error-buffer t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-show-error-buffer t
        nrepl-hide-special-buffers t
        ;; Stop error buffer from popping up while working in buffers other than the REPL:
        nrepl-popup-stacktraces nil)

  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'company-mode)

  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'superword-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-test-report-mode 'jcf-soft-wrap)

  (bind-key "C-x C-e" 'cider-eval-last-sexp clojure-mode-map)
  (bind-key "C-c C-v" 'cider-send-and-evaluate-sexp)

  :config
  (use-package slamhound))

(defun paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "*P")
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (point-at-eol))
        (setq comt (delete-and-extract-region (point) (point-at-eol)))))
    (delete-indentation arg)
    (when comt
      (save-excursion
        (move-end-of-line 1)
        (insert " ")
        (insert comt)))))

(defun paredit-remove-newlines ()
  "Removes extras whitespace and newlines from the current point
to the next parenthesis."
  (interactive)
  (let ((up-to (point))
        (from (re-search-forward "[])}]")))
     (backward-char)
     (while (> (point) up-to)
       (paredit-delete-indentation))))

(use-package paredit
  :bind ("M-^" . paredit-delete-indentation)
  :bind ("C-^" . paredit-remove-newlines)
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode))

(defun cider-send-and-evaluate-sexp ()
  "Sends the s-expression located before the point or the active
  region to the REPL and evaluates it. Then the Clojure buffer is
  activated as if nothing happened."
  (interactive)
  (if (not (region-active-p))
      (cider-insert-last-sexp-in-repl)
    (cider-insert-in-repl
     (buffer-substring (region-beginning) (region-end)) nil))
  (cider-switch-to-repl-buffer)
  (cider-repl-closing-return)
  (cider-switch-to-last-clojure-buffer)
  (message ""))

(use-package ob-clojure
  :init
  (setq org-babel-clojure-backend 'cider))

(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  ;; Configure the Clojure Refactoring prefix:
  (cljr-add-keybindings-with-prefix "C-c .")
  :diminish clj-refactor-mode)

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-clojure-docs (clojure-mode-map "C-c d" :color blue)
    "Clojure Documentation"
    ("f" cider-code "functional")
    ("g" cider-grimoire "grimoire")
    ("w" cider-grimoire-web "web examples")
    ("c" clojure-cheatsheet "cheatsheet")
    ("d" dash-at-point "dash")))

(defun ha-file-to-string (file)
  "Read the contents of FILE and return as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ha-string-to-file (string file)
  (interactive "sEnter the string: \nFFile to save to: ")
  (with-temp-file file
    (insert string)))

(defun ha-file-to-list (file)
  "Return a list of lines in FILE."
  (split-string (ha-file-to-string file) "\n" t))

(provide 'init-clojure)

;;; init-clojure.el ends here
