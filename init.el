;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(add-hook 'prog-mode-hook 'linum-mode)

;; Basic stuff to make writing code better
(electric-indent-mode +1) ; Indent new lines like the previous
(electric-pair-mode 1) ; Matching delimiters
(global-visual-line-mode 1) ; Wrap lines
(global-prettify-symbols-mode 1) ; Pretty symbols
(show-paren-mode 1) ; Show matching parens

;; Blank Scratch
(setq inhibit-startup-message t
    initial-scratch-message nil)

;; Package Configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
    ("gnu"   . "http://elpa.gnu.org/packages/")
    ("melpa" .  "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)

;; Vim Mode
(use-package evil
    :ensure t
    :config
    (evil-mode 1))
(use-package evil-escape
    :ensure t
    :init
    (setq-default evil-escape-key-sequence "jk")
    :config
    (evil-escape-mode 1))

;; Theme
(use-package dracula-theme
   :ensure t
   :config
   (load-theme 'dracula t))

;; Helm
(use-package helm
    :ensure t
    :init
    (setq helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-list 150
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
    :config
    (helm-mode 1))

;; Which Key
(use-package which-key
    :ensure t
    :init
    (setq which-key-separator " ")
    (setq which-key-prefix-prefix "+")
    :config
    (which-key-mode 1))

;; Custom Key Bindings
(use-package general
    :ensure t
    :config (general-define-key
        :states '(normal visual insert emacs)
        :prefix "SPC"
        :non-normal-prefix "M-SPC"
        "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
        "SPC" '(helm-M-x :which-key "M-x")
        "pf"  '(helm-find-files :which-key "find files")
        "bb"  '(helm-buffers-list :which-key "buffers list")
        "wl"  '(windmove-right :which-key "move right")
        "wh"  '(windmove-left :which-key "move left")
        "wk"  '(windmove-up :which-key "move up")
        "wj"  '(windmove-down :which-key "move bottom")
        "w/"  '(split-window-right :which-key "split right")
        "w-"  '(split-window-below :which-key "split bottom")
        "wx"  '(delete-window :which-key "delete window")
        "at"  '(ansi-term :which-key "open terminal")))

;; Show Matching Parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Rainbows
(use-package rainbow-delimiters
    :defer 2
    :ensure t
    :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)) ; on by default
(use-package rainbow-identifiers
    :ensure t
    :defer 2)

;; Indent Guide
(use-package indent-guide
    :defer 2
    :ensure t
    :config (indent-guide-global-mode)) ; on by default

;; Markdown Mode
(use-package markdown-mode
    :ensure t
    :defer 2
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
        ("\\.txt\\'" . markdown-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

;; Org Mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (progn
    ;; config stuff
        ))

;; Flycheck
(use-package flycheck
  :defer 2
  :ensure t
  :config(global-flycheck-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Web Mode
(use-package web-mode
  :defer 2
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;; Company
(use-package company
  :defer 2
  :ensure t
  :config (company-mode +1)
  (global-company-mode +1))

;; SLIME
(defvar LISP-VERSION "/usr/local/bin/sbcl") ; configure your preferred flavor of lisp
(use-package slime
    :defer 2
    :ensure t
    :config (setq inferior-lisp-program LISP-VERSION)
    (setq slime-contribs '(slime-fancy
        slime-sbcl-exts
        slime-cl-indent
        slime-asdf
        slime-fancy-inspector
        slime-autodoc)))

;; Are you running emacs in the terminal or in a gui?
(if (display-graphic-p)
    (set-frame-font "Hack 14") ; if emacs is running in a GUI set a big ol' font!
    (message "Terminals, FTW!"))

;; Use these packages conditionally, upon emacs version number
 (if (>= emacs-major-version 26)
    (;; Include these packages if you are running emacs 26+
    
    ;; Lumpy Space Princess! (only works on emacs 26+)
    (use-package eglot
        :defer 2
        :ensure t)

    ;; Git ╰(˙ᗜ˙)੭━☆ﾟ.*･｡ﾟ (only works on emacs 25+)
    (use-package
        magit
        :defer 2
        :ensure t))

    ;; And if you are running an older version...
    (message "You are running a version of emacs older than 26. A couple of packages haven't been installed to avoid compatibility issues."))

;; A Very Tiny User Manual
;; (condition-case err
;;     (let ((buffer (get-buffer-create "*manual*")))
;;       (with-current-buffer buffer
;;         (insert-file-contents "~/.emacs.d/README.md")
;;         (markdown-mode))
;;       (setq initial-buffer-choice buffer))
;;   (error (message "%s" error-message-string err)))

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
