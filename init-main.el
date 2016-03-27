;; initialization file (elisps/init-main.el)

;; automodes
;; shell-scripts
(add-to-list 'auto-mode-alist '("/bin/" . sh-mode))

;; ui
(when (window-system)
  (tool-bar-mode 0)               
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))

;; hydra
(use-package hydra
  :ensure t
  :config
  (hydra-add-font-lock))

;; window-resize
(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-splitter (global-map "<f9>")
  "splitter"
  ("<left>" hydra-move-splitter-left)
  ("<down>" hydra-move-splitter-down)
  ("<up>" hydra-move-splitter-up)
  ("<right>" hydra-move-splitter-right))

;; use guide-key
(use-package guide-key
  :ensure t
  :init    (setq guide-key/guide-key-sequence
                 '("C-x r"     ; rectanges and registers
                   "C-x 4"     ; window commands
                   "M-s h"     ; hi-lock highlighting
                   "C-x w"     ; alternative to M-s ...
                   "C-c @"     ; hs-hide-show mode
                   "C-c p"     ; projectile
                   "<f2>"
                   "<f9>"
                   (org-mode "C-c C-x")))
  :config  (guide-key-mode 1)
  :diminish guide-key-mode)

;; key bindings
(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-M-q") 'query-replace)
(global-set-key (kbd "<f5>") 'flyspell-mode)
(global-set-key (kbd "C-<f5>") 'linum-mode)
(global-set-key (kbd "C-<right>") 'forward-sentence)
(global-set-key (kbd "C-x c") 'calendar)
(global-set-key (kbd "C-x t") 'eshell)

;; f-keys
(global-set-key (kbd "<f5>") 'org-mark-ring-push)
(global-set-key (kbd "C-<f5>") 'org-mark-ring-goto)
(global-set-key (kbd "<f7>") 'other-window)
(global-set-key (kbd "C-<f7>") (lambda () (interactive) (other-window -1)))

;; f9 prefix
(define-prefix-command 'personal-global-map)
(global-set-key (kbd "<f9>") 'personal-global-map)

;; eshell
(setenv "ESHELL" (expand-file-name "~/bin/eshell"))

;; tramp
(setq tramp-default-method "ssh")

;; dired
(setq ls-lisp-use-insert-directory-program nil)
(use-package dired-details
  :ensure t
  :init   (setq dired-details-hidden-string "* ")
  :config (dired-details-install))

;; create dired buffer for dir-tree file search (opt)
(use-package find-dired
   :ensure t
   :init (setq find-ls-option '("-print0 | xargs -0 ls -od" . "-od")))

;; yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (dl/emacs-subdir "snippets")))

;; company
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  :diminish company-mode)

;; git
;;(use-package git-gutter-fringe
;;   :ensure t
;;   :config (git-gutter-mode 1))

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

;; mode-line
;  (require 'init-mode-line)

; __        ___     _ _
; \ \      / / |__ (_) |_ ___  ___ _ __   __ _  ___ ___
;  \ \ /\ / /| '_ \| | __/ _ \/ __| '_ \ / _` |/ __/ _ \
;   \ V  V / | | | | | ||  __/\__ \ |_) | (_| | (_|  __/
;    \_/\_/  |_| |_|_|\__\___||___/ .__/ \__,_|\___\___|
;                                 |_|

;; tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq initial-scratch-message "")
(setq visible-bell t)

;; whitespace
(use-package whitespace
  :bind ("C-c T w" . whitespace-mode)
  :init
  (setq whitespace-line-column nil
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9])))
  :config
  (set-face-attribute 'whitespace-space       nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-newline     nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-indentation nil :foreground "#666666" :background nil)
  :diminish whitespace-mode)

;; fill
(use-package fill
  :bind ("C-c T f" . auto-fill-mode)
  :init (add-hook 'org-mode-hook 'turn-on-auto-fill)
  :diminish auto-fill-mode)

;; agg autoindent

;; aggressive indent
(defun dl/indent-defun ()
  "Indent current defun.
   Do nothing if mark is active (to avoid deactivaing it), or if
   buffer is not modified (to avoid creating accidental
   modifications)."
  (interactive)
  (unless (or (region-active-p)
              buffer-read-only
              (null (buffer-modified-p)))
    (let ((l (save-excursion (beginning-of-defun 1) (point)))
          (r (save-excursion (end-of-defun 1) (point))))
      (cl-letf (((symbol-function 'message) #'ignore))
        (indent-region l r)))))
(defun activate-aggressive-indent ()
  "Locally add `dl/indent-defun' to `post-command-hook'."
  (add-hook 'post-command-hook
            'indent-defun nil 'local))

(require 'init-clojure)
(require 'init-org)
(require 'init-web)

;; smartparens
;(use-package smartparens
;  :init
;  (add-hook 'org-mode-hook 'smartparens-strict-mode)
;  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
;  (add-hook 'markdown-mode--hok 'smartparens-strict-mode))

(provide 'init-main)

;;; init-main.el ends here


