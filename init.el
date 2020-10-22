;;; init.el --- Initialization file for Emacs
;;; Commentary:
;; Eric Lai
;; - git must be installed
;; - the straight.el bootstrap must be before any other package
;; - use-package must be installed right after straight

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight.el bootstrap		   		   ;;
;; https://github.com/raxod502/straight.el ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package							   ;;
;; config management					   ;;
;; https://github.com/jwiegley/use-package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(straight-use-package 'use-package)
;; use-package will use straight.el to automatically install
;; missing packages if provided with ':straight t'


;;;;;;;;;;;;;;;;;;;;;
;; better defaults ;;
;;;;;;;;;;;;;;;;;;;;;
(global-linum-mode t)
(setq-default tab-width 4)
(tool-bar-mode 0)
(toggle-scroll-bar 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common lisp compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cl-lib
  :straight t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sr Speedbar								  					    ;;
;; speedbar in the same fram				  					    ;;
;; https://www.emacswiki.org/emacs/SrSpeedbar 					    ;;
;; https://www.gnu.org/software/emacs/manual/html_node/speedbar/    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sr-speedbar
  :straight t
  :bind ([f8] . sr-speedbar-toggle)
  :custom
  (speedbar-use-images nil "disable icons")
  (speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'"))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window							 ;;
;; better window switching				 ;;
;; https://github.com/abo-abo/ace-window ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window)
  :custom
  (aw-dispatch-always 1))


;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;
(use-package moe-theme
  :straight t
  :config
  (moe-theme-set-color 'magenta)
  (moe-dark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HideShow				 ;;
;; code folding, builtin ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-selective-display (column)
  ;; hides code based on indentation level
  (interactive "P")
  (set-selective-display
   (or column
	   (unless selective-display
		 (1+ (current-column))))))

(defun toggle-hiding (column)
  ;; smart hiding, uses selective-display as fallback
  (interactive "P")
  (if hs-minor-mode
	  (if (condition-case nil
			  (hs-toggle-hiding)
			(error t))
		  (hs-show-all))
	(toggle-selective-display column)))

(load-library "hideshow")
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Backups/Autosave ;;
;; change location		 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile							 ;;
;; project management					 ;;
;; https://github.com/bbatsov/projectile ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom
  (projectile-completion-system 'ivy "use ivy for projectile completion backend"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counsel - Ivy, Swiper			 ;;
;; completion interface				 ;;
;; https://github.com/abo-abo/swiper ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :straight t
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "[%d/%d] "))


;;;;;;;;;;;;;;;;;;;;;;;
;; Magit			 ;;
;; git interface	 ;;
;; https://magit.vc/ ;;
;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck										 ;;
;; syntax checking								 ;;
;; https://www.flycheck.org/en/latest/index.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 24.3+, disable if buggy on windows
(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode))


;;;;;;;;;;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook 'hs-minor-mode) ;; elpy code folding compatibility

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elpy	(multiple dependencies)			 ;;
;; python development environment		 ;;
;; https://elpy.readthedocs.io/en/latest ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elpy
  :straight t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable) ;; defer loading
  :config
  (when (load "flycheck" t t)  ;; use flycheck instead of flymake for syntax checking backend
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	(add-hook 'elpy-mode-hook 'flycheck-mode))
  :custom
  (elpy-folding-fringe-indicators t "enable code folding fringe indicators")
  (elpy-modules
   '(elpy-module-company
  	 elpy-module-eldoc
  	 elpy-module-flymake
  	 elpy-module-folding
  	 elpy-module-pyvenv
  	 elpy-module-highlight-indentation
  	 elpy-module-yasnippet
  	 elpy-module-django
  	 elpy-module-sane-defaults) "activate elpy modules")
  :custom-face
  (elpy-folding-fringe-face ((t (:inherit (quote font-lock-keyword-face) :box (:line-width 1 :style released-button))))))
;; run elpy-config to get external dependencies
;; might want to switch to flycheck in the future


;;;;;;;;;;;;;;;;;;;;;;;;; LaTeX ;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUCTeX										  ;;
;; support for TeX and TeX macro packages		  ;;
;; https://www.gnu.org/software/auctex/index.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tex ;;workaround because auctex is old
  :straight auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t))
  
;;;;;;;;;;;;;;;;;;;;;;
;; still needed:    ;;
;; markdown preview ;;
;; org mode config  ;;
;; Ocaml    	    ;;
;; c/c++		    ;;
;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;; Org Mode ;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GTD Setup					   ;;
;; task keeping/project management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq gtd-files '("~/Dropbox/gtd/inbox.org"
				  "~/Dropbox/gtd/gtd.org"
				  "~/Dropbox/gtd/reminders.org"
				  "~/Dropbox/gtd/someday.org"))

(defun check-exists (list)
  "t if all files in 'list' exist"
  (eval `(and ,@(mapcar
				 (lambda (filename) (file-exists-p filename))
				 list))))


(when (check-exists gtd-files)
  (setq org-agenda-files '("~/Dropbox/gtd/inbox.org"
						   "~/Dropbox/gtd/gtd.org"
						   "~/Dropbox/gtd/reminders.org"))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
								 (file+headline "~/Dropbox/gtd/inbox.org" "Tasks")
								 "* TODO %i%?")
								("r" "Reminder" entry
								 (file+headline "~/Dropbox/gtd/reminders.org" "Reminder")
								 "* %i%? \n %U")))
  (setq org-refile-targets '(("~/Dropbox/gtd/gtd.org" :maxlevel . 3)
							 ("~/Dropbox/gtd/someday.org" :level . 1)
							 ("~/Dropbox/gtd/reminders.org" :maxlevel . 2)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-agenda-custom-commands 
		'(("o" "At the office" tags-todo "@office"
		   ((org-agenda-overriding-header "Office")
			(org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))	
  (defun my-org-agenda-skip-all-siblings-but-first ()
	"Skip all but the first non-done entry."
	(let (should-skip-entry)
	  (unless (org-current-is-todo)
		(setq should-skip-entry t))
	  (save-excursion
		(while (and (not should-skip-entry) (org-goto-sibling t))
		  (when (org-current-is-todo)
			(setq should-skip-entry t))))
	  (when should-skip-entry
		(or (outline-next-heading)
			(goto-char (point-max))))))
  (defun org-current-is-todo ()
	(string= "TODO" (org-get-todo-state))))
	  

;; disable flycheck for this file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((eval visual-line-mode t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-folding-fringe-face ((t (:inherit 'font-lock-keyword-face :box (:line-width 1 :style released-button))))))
;; Local Variables:
;; flycheck-disabled-checkers: emacs-lisp-checkdoc
;; End:
