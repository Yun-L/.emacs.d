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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree	     				 				  ;;
;; directory sidebar 				  			  ;;
;; https://github.com/jaypei/emacs-neotree	  	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package neotree
;;   :straight t
;;   :bind ([f8] . neotree-toggle)
;;   :custom
;;   (neo-theme (if (display-graphic-p) 'ascii))
;;   (neo-smart-open t "opens at current buffer's file location")
;;   :custom-face
;;   (neo-expand-btn-face ((t (:background "#FFFFFF")))) ;; replace with light theme color
;;   (neo-expand-btn-face ((((background dark)) (:background "#303030")))))


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
  (moe-light)
  (require 'moe-theme-switcher)
  :custom
  (calendar-latitude +40 "theme mode switching (sunrise/sunset)")
  (calendar-longitude -74))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symon						    ;;
;; system monitor in minibuffer	    ;;
;; https://github.com/zk-phi/symon/ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package symon
  :straight t
  :config
  (symon-mode))


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

;; disable flycheck for this file

;; Local Variables:
;; flycheck-disabled-checkers: emacs-lisp-checkdoc
;; End:
