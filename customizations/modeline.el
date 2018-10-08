
;; Configurations and Customizations for the emacs modeline to make it
;; more ledigable and contain the information we actually find 
;; important. 

;; Dont show minor modes in modeline since we can find them in other ways.
;; (setq mode-line-modes
;;       (mapcar (lambda (elem)
;;                 (pcase elem
;;                   (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
;;                    "")
;;                   (t elem)))
;;              mode-line-modes))

;; Setup fancy icons for git
;; (defadvice vc-mode-line (after strip-backend () activate)
;;   (when (stringp vc-mode)
;;     (let ((gitlogo (replace-regexp-in-string "Git" "" vc-mode)))
;;           (setq vc-mode gitlogo))))

;; (defadvice vc-mode-line (after strip-backend () activate)
;;   (when (stringp vc-mode)
;;     (let ((commitlogo (replace-regexp-in-string "-" "" vc-mode)))
;;           (setq vc-mode commitlogo))))

;; (defadvice vc-mode-line (after strip-backend () activate)
;;   (when (stringp vc-mode)
;;     (let ((branchlogo (replace-regexp-in-string ":" "" vc-mode)))
;;           (setq vc-mode branchlogo))))

(doom-modeline-init)


;; Battery indicator


