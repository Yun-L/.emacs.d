
;; Add Melpa Source
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Initialize Package
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (multiple-cursors magit moe-theme))))
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

;; multiple cursors

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; General Config
(global-linum-mode t)
(setq-default tab-width 4)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
