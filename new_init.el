;; move configs to new_config once done and add docs

;; (require 'org)
;; (org-babel-load-file
;;  (expand-file-name "new_config.org"
;; 				   user-emacs-directory))

;NOTE: use descripe-variable/function to see what .el file it comes from, to find out the package name (provide ...)

;;;; hacks to get stuff working temporarily
;; replacing c/cpp modes with tree sitter compatible ones
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))




;; include for now but plans to modify or do on my own
(use-package ace-window
  :load-path "external_packages/ace-window/"
  :after (avy)
  :bind ("M-o" . ace-window)
  :custom
  ;; keep same behavior even with only 2 windows open
  (aw-dispatch-always 1))
(use-package avy
  :load-path "external_packages/avy/"
  :demand t
  :bind
  (("C-:" . avy-goto-char)
   ("C-;" . avy-goto-char-2))
  :custom
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s) "change to dvorak home row keys"))


(use-package gruvbox-theme
  :load-path "external_packages/emacs-theme-gruvbox/"
  :after (autothemer)
  ;; :init
  ;; (add-to-list 'custom-theme-load-path "external_packages/emacs-theme-gruvbox/")
  :config (load-theme 'gruvbox-light-soft t))
(use-package dash ;; dependency for gruvbox-theme, magit
  :load-path "external_packages/dash.el/")
(use-package autothemer ;; dependency for gruvbox-theme
  :after (dash)
  :load-path "external_packages/autothemer/")


(use-package undo-tree
  :load-path "external_packages/undo-tree/"
  :after (queue)
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
(use-package queue ;; dependency for undo-tree
  :load-path "external_packages/queue/")


(use-package magit
  :load-path "external_packages/magit/lisp/"
  :after (dash with-editor compat xah-fly-keys) ; transient? (not installed but still works)
  ;; :hook (magit-popup-mode . xah-fly-insert-mode-activate)
  :bind
  ("C-x g" . magit-status)
  (:map magit-file-section-map
        ("RET" . magit-diff-visit-file-other-window)
        :map magit-hunk-section-map
        ("RET" . magit-diff-visit-file-other-window)))
(use-package compat ;; dependency for magit
  :load-path "external_packages/compat/")
(use-package with-editor ;; dependency for magit
  :load-path "external_packages/with-editor/lisp/")

(use-package projectile
  :load-path "external_packages/projectile/"
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom
  (projectile-enable-caching t))
;; conditionally use external tools, ripgrep, ag etc

(use-package persp-mode
  :load-path "external_packages/persp-mode.el/"
  :config
  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1)))
  :bind
  ("C-x b" . #'persp-switch-to-buffer)
  ("C-x k" . #'persp-kill-buffer)
  :custom
  (persp-keymap-prefix (kbd "C-c w"))
  (persp-autokill-buffer-on-remove 'kill-weak))
(use-package persp-mode-projectile-bridge
  :load-path "external_packages/persp-mode-projectile-bridge.el/"
  :after (persp-mode projectile)
  :config
  (with-eval-after-load "persp-mode-projectile-bridge-autoloads"
    (add-hook 'persp-mode-projectile-bridge-mode-hook
              #'(lambda ()
                  (if persp-mode-projectile-bridge-mode
                      (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                    (persp-mode-projectile-bridge-kill-perspectives))))
    (add-hook 'after-init-hook
              #'(lambda ()
                  (persp-mode-projectile-bridge-mode 1))
              t)))
  

(use-package xah-fly-keys
  :load-path "external_packages/xah-fly-keys/"
  :init
  (setq xah-fly-use-control-key nil)
  (setq xah-fly-use-meta-key nil)
  :config
  (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
  (xah-fly-keys-set-layout "dvorak")
  (xah-fly-keys 1))


(use-package yasnippet
  :load-path "external_packages/yasnippet/"
  :config
  (yas-global-mode 1))


;; built in
(use-package compile
  :config
  (fringe-mode nil)
  (setq window-divider-default-places t)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-right-width 1)
  (window-divider-mode)
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (setq frame-resize-pixelwise t)
  (setq-default frame-title-format '("emacs-yun"))
  (add-to-list
   'default-frame-alist
   ;; "-outline-InputMono-regular-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
   '(font . "-outline-InputMono Medium-medium-normal-normal-mono-19-*-*-*-c-*-iso10646-1"))
  ;; (when
  
  ;;     (find-font (font-spec :name "InputMono-12"))
  ;;   (set-frame-font "InputMono-12" t t)
  ;;   (custom-theme-set-faces
  ;;    'user
  ;;    '(variable-pitch ((t (:family "Input Serif"))))
  ;;    '(fixed-pitch ((t ( :family "Input Mono")))))
  ;;   )
  (defun find-user-config-file ()
    (interactive)
    (if (file-exists-p "~/.emacs.d/config.org")
        (find-file "~/.emacs.d/config.org")
      (error "%s" "no config.org file found in ~/.emacs.d/")))

  (global-set-key (kbd "C-~") 'find-user-config-file)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq ring-bell-function 'ignore)
  (setq inhibit-startup-screen t)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (if (file-directory-p "~/.emacs.d/backup/")
      nil
    (make-directory "~/.emacs.d/backup/"))
  (if (file-directory-p "~/.emacs.d/autosave/")
      nil
    (make-directory "~/.emacs.d/autosave/"))
  (defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
  (defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
  (setq backup-directory-alist (list (cons ".*" backup-dir)))
  (setq auto-save-list-file-prefix autosave-dir)
  (setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (unless (version< emacs-version "27.1")
    (setq-default display-fill-column-indicator-column 80)
    (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq c-basic-offset 4)
              (c-set-offset 'case-label '+)))
  )

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("localhost" 50505))) ; needs better solution than hardcoded port
  (global-set-key (kbd "M-.") 'xref-find-definitions)
  (global-set-key (kbd "M-,") 'xref-go-back)
  (global-set-key (kbd "M-p") 'xref-find-references))

;; (use-package tree-sitter), build lib and compatibility manually but shouldn't need configs

(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-startup-folded nil)
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (set-face-attribute 'org-document-title nil :weight 'bold :font "InputSans" :height 2.0)
  (set-face-attribute 'org-level-1 nil :weight 'bold :font "InputSans" :height 1.75)
  (set-face-attribute 'org-level-2 nil :weight 'bold :font "InputSans" :height 1.5)
  (set-face-attribute 'org-level-3 nil :weight 'bold :font "InputSans" :height 1.25)
  (set-face-attribute 'org-level-4 nil :weight 'bold :font "InputSans" :height 1.00)
  (set-face-attribute 'org-level-5 nil :weight 'bold :font "InputSans" :height 1.00)
  (set-face-attribute 'org-level-6 nil :weight 'bold :font "InputSans" :height 1.00)
  (set-face-attribute 'org-level-7 nil :weight 'bold :font "InputSans" :height 1.00)
  (set-face-attribute 'org-level-8 nil :weight 'bold :font "InputSans" :height 1.00)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch :extend t)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-document-info nil :foreground "dark orange")
  (set-face-attribute 'org-document-info-keyword nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch :foreground "#83a598")
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch :background (face-background 'org-table))
  (set-face-attribute 'org-tag nil :inherit '(shadow fixed-pitch) :weight 'bold :height 0.8)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch)

  ;; need to do this because org-indent face may not exist yet
  (custom-theme-set-faces
   'user
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))))

;;;; definitely want
;; (use-package yasnippet)
;; (use-package yasnippet-snippets)

;;;; don't really want but used for work
;; (use-package prettier)
;; (use-package web-mode) ;; is there a better way for editing ember files?
;; (use-package cmake)

;; mode defined in c-ts-mode.el:)

;;;; unsure what to do with these
;; (use-package sr-speedbar)
;; (use-package prescient)
;; (use-package latex/tex)
;; (use-package ivy)
;; (use-package counsel)
;; (use-package swiper)
;; (use-package smart-mode-line)
;; (use-package diminish)
;; (use-package transpose-frame)

;;;; maybe useful at work?
;; (use-package atomic-chrome)
