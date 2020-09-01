;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symon						    ;;
;; system monitor in minibuffer	    ;;
;; https://github.com/zk-phi/symon/ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package symon
  :straight t
  :config
  (symon-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree	     				 				  ;;
;; directory sidebar 				  			  ;;
;; https://github.com/jaypei/emacs-neotree	  	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package neotree
  :straight t
  :bind ([f8] . neotree-toggle)
  :custom
  (neo-theme (if (display-graphic-p) 'ascii))
  (neo-smart-open t "opens at current buffer's file location")
  :custom-face
  (neo-expand-btn-face ((t (:background "#FFFFFF")))) ;; replace with light theme color
  (neo-expand-btn-face ((((background dark)) (:background "#303030")))))
