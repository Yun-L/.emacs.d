
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
 '(package-selected-packages (quote (magit moe-theme))))
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


;; General Config
(global-linum-mode t)
(setq-default tab-width 4)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
