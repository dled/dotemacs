;;; elisp/init-org.el
;; add journaling * org-michel * google-api-python-client
;; add org/ox-reveal // org-tree-slide | presentations
(use-package org
  :ensure t        ; But it comes with Emacs now!?
  :init
  (setq org-use-speed-commands t
        org-hide-emphasis-markers t
        org-src-fontify-natively t   ;; Pretty code blocks
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))   ;; Journal entries
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-M-|" . indent-rigidly))
  :config
  (define-key org-mode-map (kbd "M-C-n") 'org-end-of-item-list)
  (define-key org-mode-map (kbd "M-C-p") 'org-beginning-of-item-list)
  (define-key org-mode-map (kbd "M-C-u") 'outline-up-heading)
  (define-key org-mode-map (kbd "M-C-w") 'org-table-copy-region)
  (define-key org-mode-map (kbd "M-C-y") 'org-table-paste-rectangle)

  (define-key org-mode-map [remap org-return] (lambda () (interactive)
                                                (if (org-in-src-block-p)
                                                    (org-return)
                                                  (org-return-indent)))))

(use-package org-plus-contrib
  :ensure t)

(defun org-text-bold () "Wraps the region with asterisks."
  (interactive)
  (surround-text "*"))
(defun org-text-italics () "Wraps the region with slashes."
  (interactive)
  (surround-text "/"))
(defun org-text-code () "Wraps the region with equal signs."
  (interactive)
  (surround-text "="))

(add-hook 'org-mode-hook
      (lambda ()
        (local-set-key (kbd "A-b") 'org-text-bold)
        (local-set-key (kbd "s-b") 'org-text-bold)    ;; For Linux
        (local-set-key (kbd "A-i") 'org-text-italics)
        (local-set-key (kbd "s-i") 'org-text-italics)
        (local-set-key (kbd "A-=") 'org-text-code)
        (local-set-key (kbd "s-=") 'org-text-code)))

(use-package org-bullets
   :ensure t
   :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-mode
  :init
  (font-lock-add-keywords 'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package ox-html
  :init
  (setq org-html-postamble nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-html-head-extra "
     <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic&subset=latin,latin-ext' rel='stylesheet' type='text/css'>
     <link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:400,700' rel='stylesheet' type='text/css'>
     <style type='text/css'>
        body {
           font-family: 'Source Sans Pro', sans-serif;
        }
        pre, code {
           font-family: 'Source Code Pro', monospace;
        }
     </style>"))

;; tree slide
(use-package org-tree-slide
   :ensure t
   :init
   (setq org-tree-slide-skip-outline-level 4)
   (org-tree-slide-simple-profile))


;; LP w/ babel
;; http://orgmode.org/worg/org-contrib/babel/intro.html
(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh         . t)
     (js         . t)
     (emacs-lisp . t)
     (perl       . t)
     (scala      . t)
     (clojure    . t)
     (python     . t)
     (ruby       . t)
     (dot        . t)
     (css        . t)
     (plantuml   . t))))

;; exit src block edit
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     (kbd "C-x C-s") #'org-edit-src-exit))

(setq org-confirm-babel-evaluate nil)

;; native syntax render
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(provide 'init-org)

;;; init-org.el ends here
