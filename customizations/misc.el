;;; misc.el --- miscelanious settings
;; Changes all yes/no questions to y/n type

;;; Commentary:
;; 

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; fixes for variable binding depth
;; (setq max-specpdl-size 100000)
;; (setq max-lisp-eval-depth 100000)

(provide 'misc)

;;; misc.el ends here
