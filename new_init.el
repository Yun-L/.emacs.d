;; (require 'org)
;; (org-babel-load-file
;;  (expand-file-name "new_config.org"
;; 				   user-emacs-directory))

;; built in
(use-package eglot)
(use-package tree-sitter)
(use-package eglot)
(use-package org)
(use-package adaptive-wrap)
(use-package InputMono font)
(use-package cpp/c)

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
;; (use-package ace-window)
;; (use-package avy)
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

;; unsure what to do with
(use-package sr-speedbar)
(use-package company)
(use-package prescient)
(use-package latex/tex)
(use-package counsel-etags)
(use-package atomic-chrome)
(use-package hyperbole)
(use-package highlight)

;; definitely want
(use-package undo-tree)
(use-package magit)
(use-package yasnippet)
(use-package yasnippet-snippets)

;; don't really want but used for work
(use-package prettier)
(use-package web-mode) ;; is there a better way for editing ember files?
(use-package cmake)





