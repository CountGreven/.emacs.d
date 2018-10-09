;;; elisp-editing.el --- settings to make editing lisp smoother
;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit

;;; Commentary:
;; 

;;; Code:

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Execute and replace last sexp.
;; Replaces the previous sexp with the result of its evaluation

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Bind evaluate buffer to "C-c b"

(global-set-key (kbd "C-c b") 'eval-buffer)

;; Flymake should not treat every elisp file as a package
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; Bad idea, to automatically checkdock any elisp buffer
;; (add-hook 'emacs-lisp-mode-hook 'checkdoc)
(global-set-key (kbd "C-c d") 'checkdoc)
(provide 'elisp-editing)

;;; elisp-editing.el ends here
