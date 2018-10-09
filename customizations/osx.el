;;; osx.el --- Configurations for mac osX

;;; Commentary:
;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during then
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell

(add-to-list 'my-packages 'exec-path-from-shell)
;; Set cmd as meta in OS X, and also make sure we can use opt
;; for special characters such as "|".

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'nil)

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))


(provide 'osx)

;;; osx.el ends here
