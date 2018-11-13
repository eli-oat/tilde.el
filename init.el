;; Collect *ALL* the garbage
(setq gc-cons-threshold 100000000)

;; Collect slightly less garbage while running
(add-hook 'after-init-hook 
    (lambda ()
        (setq gc-cons-threshold 400000)))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)

(load "~/.emacs.d/tilde.el")

(when (file-exists-p "~/.emacs.d/tree-fort.el")
    (load "~/.emacs.d/tree-fort.el"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (deft which-key web-mode use-package slime rainbow-identifiers rainbow-delimiters projectile org-plus-contrib markdown-mode magit indent-guide helm general flycheck evil-escape evil eglot dracula-theme company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
