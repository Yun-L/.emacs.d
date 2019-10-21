
;; Add Melpa Source
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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
 '(package-selected-packages
   (quote
	(impatient-mode gnu-elpa-keyring-update flycheck elpy multiple-cursors magit moe-theme))))
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

;;;;;;;;;;; CONFIGS FOR PACKAGES

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
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;;;; General Config

(global-linum-mode t)
(setq-default tab-width 4)
(setq-default fill-column 80)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(tool-bar-mode -1)
(toggle-scroll-bar -1)


;; Backup/Autosave

(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; fixed error in emacs versions <= 26.2
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; impatient mode function for markdown files
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
		   (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
		 (current-buffer)))
