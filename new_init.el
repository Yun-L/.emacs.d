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


(use-package avy
  :load-path "external_packages/avy/"
  :bind
  (("C-:" . avy-goto-char)
   ("C-;" . avy-goto-char-2))
  :custom
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s) "change to dvorak home row keys"))


;; include for now but plans to modify or do on my own
(use-package ace-window
  :load-path "external_packages/ace-window/"
  :after (avy)
  :commands ace-window
  :bind ("M-o" . ace-window)
  :custom
  ;; keep same behavior even with only 2 windows open
  (aw-dispatch-always 1))



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
  (when
      (find-font (font-spec :name "Input Mono"))
    (set-frame-font "Input Mono-10" t t)
    (custom-theme-set-faces
     'user
     '(variable-pitch ((t (:family "Input Serif"))))
     '(fixed-pitch ((t ( :family "Input Mono")))))
    )
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
;; (use-package undo-tree)
;; (use-package magit)
;; (use-package yasnippet)
;; (use-package yasnippet-snippets)

;;;; don't really want but used for work
;; (use-package prettier)
;; (use-package web-mode) ;; is there a better way for editing ember files?
;; (use-package cmake)

;; mode defined in c-ts-mode.el:)

;;;; unsure what to do with these
;; (use-package sr-speedbar)
;; (use-package company)
;; (use-package prescient)
;; (use-package latex/tex)
;; (use-package counsel-etags)
;; (use-package atomic-chrome)
;; (use-package hyperbole)
;; (use-package highlight)

;;;; do myself or remove
;; (use-package smart-mode-line)
;; (use-package diminish)
;; (use-package gruvbox-theme)
;; (use-package which-key)
;; (use-package transpose-frame)
;; (use-package treemacs)
;; (use-package treemacs-magit)
;; (use-package projectile)
;; (use-package treemacs-projectile)
;; (use-package persp-mode)
;; (use-package persp-mode-projectile-bridge)
;; (use-package ivy)
;; (use-package counsel)
;; (use-package swiper)
;; (use-package dump-jump)
;; (use-package ivy-prescient)
;; (use-package company-prescient)
;; (use-package csharp)
;; (use-package lua)
;; (use-package markdown)
;; (use-package docker-compose)
;; (use-package xah-fly-keys)
;; (use-package docker)
;; (use-package lsp-docker)
;; (use-package java)
;; (use-package typescript)
;; (use-package uml)
;; (use-package adaptive-wrap)
