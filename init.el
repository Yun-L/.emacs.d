
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
	(auctex impatient-mode gnu-elpa-keyring-update flycheck elpy multiple-cursors magit moe-theme))))
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
(add-hook 'after-change-major-mode-hook 'fci-mode)

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
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; impatient mode function for markdown files
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
		   (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
		 (current-buffer)))

;; Org Mode
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-log-done 'time)
(setq org-log-done 'note)

;; Org Mode - Task Management

(setq org-agenda-files '("~/Dropbox/org/gtd_curr.org"
						 "~/Dropbox/org/inbox.org"
						 "~/Dropbox/org/reminders.org"))


(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("r" "Reminder" entry
                               (file+headline "~/Dropbox/org/reminders.org" "Reminder")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/Dropbox/org/gtd_curr.org" :maxlevel . 5)
                           ("~/Dropbox/org/someday.org" :level . 1)
                           ("~/Dropbox/org/reminders.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))

;; OCaml
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

(add-to-list 'load-path "/home/yun/.opam/system/share/emacs/site-lisp")
(require 'ocp-indent)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line


;; auctex
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTex t)
(global-font-lock-mode t)
