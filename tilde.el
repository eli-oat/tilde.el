;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(cua-mode t nil (cua-base)) ; allow for normal copy/paste behavior (because I am a monster)
(add-hook 'prog-mode-hook 'linum-mode)

;; Display the menu bar *if* and only if in GUI.
(if (display-graphic-p)
    (progn
      (menu-bar-mode 1))
  (menu-bar-mode -1))

;; Basic stuff to make writing code better
(electric-indent-mode +1) ; Indent new lines like the previous
(electric-pair-mode 1) ; Matching delimiters
(global-visual-line-mode 1) ; Wrap lines
(global-prettify-symbols-mode 1) ; Pretty symbols

; Show Matching Parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Blank Scratch
(setq inhibit-startup-message t
    initial-scratch-message nil)

;; Vim Mode
(use-package evil
    :ensure t
    :config (evil-mode 1))
(use-package evil-escape
    :ensure t
    :init
    (setq-default evil-escape-key-sequence "jk")
    :config (evil-escape-mode 1))

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
    :config (helm-mode 1))

;; Which Key
(use-package which-key
    :ensure t
    :init
    (setq which-key-separator " ")
    (setq which-key-prefix-prefix "+")
    :config (which-key-mode 1))

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

;; Rainbows
(use-package rainbow-delimiters
    :ensure t
    :defer 2
    :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)) ; on by default
(use-package rainbow-identifiers
    :ensure t
    :defer 2)

;; Indent Guide
(use-package indent-guide
    :ensure t
    :defer 2
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
  :ensure t
  :defer 2)

;; Flycheck
(use-package flycheck
    :ensure t
    :defer 2
    :config (global-flycheck-mode))
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
