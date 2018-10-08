;; Settings for pdf viewing, using pdf-tools.

(pdf-tools-install)

;;Open pdf's scaled to fit page

(setq-default pdf-view-display-size 'fit page)

;; automatically annotate highlights
(setq pdf-annot-activate-created-annotations t)

;; use normal isearch
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward) 
