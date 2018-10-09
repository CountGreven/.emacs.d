;;; custom-org.el --- Special configurations for org-mode
;;; Commentary:
;; Enable bable for elisp, shell and python

;;; Code:
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))

;; Disable verbose confirmation for babel evaluation.
;; This is potentially dangerous. You should never evaluate code
;; if you do not understand what it does.

( setq-default org-confirm-babel-evaluate nil)

;; Org-bullets
;; Make the org-mode organization of headers a bit prettier

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(provide 'custom-org)
;;; custom-org.el ends here
