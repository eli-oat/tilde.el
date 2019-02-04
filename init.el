;; Collect *ALL* the garbage
(setq gc-cons-threshold 100000000)

;; Collect slightly less garbage while running to improve performance
(add-hook 'after-init-hook
    (lambda ()
        (setq gc-cons-threshold 400000)))

;; Run emacs server
(server-start)

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

;; Load the config
(load "~/.emacs.d/tilde.el")

;; If tree-fort.el exists import it. Use tree-fort.el for personal config stuff
(when (file-exists-p "~/.emacs.d/tree-fort.el")
    (load "~/.emacs.d/tree-fort.el"))
