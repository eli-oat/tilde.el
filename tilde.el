;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(menu-bar-mode    t) ; I like the menu bar?
(tooltip-mode    -1)
(add-hook 'prog-mode-hook 'linum-mode)

;; Basic stuff to make writing code better
(electric-indent-mode        +1) ; Indent new lines like the previous
(electric-pair-mode           1) ; Matching delimiters
(global-visual-line-mode      1) ; Wrap lines
(global-prettify-symbols-mode 1) ; Pretty symbols

; Show Matching Parens
(setq show-paren-delay 0)
(show-paren-mode       1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Blank Scratch
(setq inhibit-startup-message t
    initial-scratch-message nil)

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

;; Rainbows
(use-package rainbow-delimiters
    :ensure t
    :defer 2
    :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)) ; on by default
(use-package rainbow-identifiers
    :ensure t
    :defer 2)

;; Highlight TODO and FIXME
(use-package hl-todo
  :ensure t
  :defer 2
  :config (add-hook 'prog-mode-hook #'hl-todo-mode))

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

;; Golden Ratio
(use-package golden-ratio
  :defer 2
  :ensure t
  :config (golden-ratio-mode 1))
