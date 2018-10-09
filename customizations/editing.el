;;; editing.el --- Customizations relating to editing a buffer.

;;; Commentary:

;;; Code:

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand

(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace

(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-c ;") 'toggle-comment-on-line)
(global-set-key (kbd "C-c #") 'comment-or-uncomment-region)

;; yay rainbows!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

;; disable transient mark mode becuase transient mark mode is for idiots
(transient-mark-mode 0) 

;; Setup git-gutter+ to get visual feedback about the git status of the file we are working on

(global-git-gutter+-mode)

;; Keybindings to turn on and off git-gutter+-mode. Probably wont use these but keep them around 
;; in case we may need them later.

;;  (global-set-key (kbd "C-x g") 'git-gutter+-mode) ; Turn on/off in the current buffer
;;  (global-set-key (kbd "C-x G") 'global-git-gutter+-mode) ; Turn on/off globally

  (eval-after-load 'git-gutter+
    '(progn
       ;;; Jump between hunks
       (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
       (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)

       ;;; Act on hunks
       (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
       (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)

       ;; Stage hunk at point.
       ;; If region is active, stage all hunk lines within the region.

       (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
       (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
       (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
       (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
       (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)))

;; Setup keybinding for git-timemachine
(global-set-key (kbd "C-c t") 'git-timemachine)

;; Company mode for autocompletions
(add-hook 'after-init-hook 'global-company-mode)

;;Yasnippet for snippets
(yas-global-mode 1)

;; Auto-yasnippet to create snippets automatically
(global-set-key (kbd "C-c y") #'aya-create)
(global-set-key (kbd "C-c x") #'aya-expand)

;; Flycheck mode for syntax highlighting and linting
;;(add-hook 'after-init-hook #'global-flycheck-mode)


(provide 'editing)

;;; editing.el ends here
