
;; Add Melpa Source
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Add Local Directories to Load Path
(add-to-list 'load-path "~/.emacs.d/extras")

;; Initialize Package
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-rpc-python-command "python")
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (flycheck elpy multiple-cursors magit moe-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Moe Theme Mode Switching (Sunrise/Sunset)
(require 'moe-theme-switcher)
(setq calendar-latitude +40)
(setq calendar-longitude -74)

;; ##### CONFIGS FOR PACKAGES#####

;; Multiple Cursors

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Fill Column Indicator

(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "turquoise1")


;; Python Development

(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; General Config

(global-linum-mode t)
(setq-default tab-width 4)
(setq-default fill-column 80)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

