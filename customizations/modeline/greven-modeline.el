;;; greven-modeline.el --- A minimal and modern modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'all-the-icons)
(require 'eldoc-eval)
(require 'projectile)
(require 'shrink-path)


;;
;; Variables
;;

(defvar greven-modeline-height 25
  "How tall the mode-line should be (only respected in GUI Emacs).")

(defvar greven-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI Emacs).")

(defvar greven-modeline-buffer-file-name-style 'truncate-upto-project
  "Determines the style used by `greven-modeline-buffer-file-name'.

Given ~/Projects/FOSS/emacs/lisp/comint.el
  truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  truncate-upto-root => ~/P/F/e/lisp/comint.el
  truncate-all => ~/P/F/e/l/comint.el
  relative-from-project => emacs/lisp/comint.el
  relative-to-project => lisp/comint.el
  file-name => comint.el")

(defvar greven-modeline-python-executable "python"
  "What executable of Python will be used (if nil nothing will be shown).")

;; externs
(defvar anzu--current-position)
(defvar anzu--overflow-p)
(defvar anzu--state)
(defvar anzu--total-matched)
(defvar anzu-cons-mode-line-p)
(defvar evil-ex-active-highlights-alist)
(defvar evil-ex-argument)
(defvar evil-ex-range)
(defvar evil-mode)
(defvar evil-state)
(defvar evil-emacs-state-tag)
(defvar evil-insert-state-tag)
(defvar evil-motion-state-tag)
(defvar evil-normal-state-tag)
(defvar evil-operator-state-tag)
(defvar evil-replace-state-tag)
(defvar evil-visual-state-tag)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-visual-selection)
(defvar flycheck-current-errors)
(defvar iedit-mode)
(defvar iedit-occurrences-overlays)
(defvar text-scale-mode-amount)
(defvar winum-auto-setup-mode-line)
(defvar mc/mode-line)

(declare-function anzu--reset-status 'anzu)
(declare-function anzu--where-is-here 'anzu)
(declare-function eldoc-in-minibuffer-mode 'eldoc-eval)
(declare-function evil-delimited-arguments 'evil-common)
(declare-function evil-state-property 'evil-common)
(declare-function eyebrowse--get 'eyebrowse)
(declare-function face-remap-remove-relative 'face-remap)
(declare-function flycheck-count-errors 'flycheck)
(declare-function iedit-find-current-occurrence-overlay 'iedit-lib)
(declare-function iedit-prev-occurrence 'iedit-lib)
(declare-function image-get-display-property 'image-mode)
(declare-function magit-toplevel 'magit-git)
(declare-function window-numbering-clear-mode-line 'window-numbering)
(declare-function window-numbering-get-number-string 'window-numbering)
(declare-function window-numbering-install-mode-line 'window-numbering)
(declare-function winum-get-number-string 'winum)

;;
;; Custom faces
;;

(defgroup greven-modeline nil
  "Greven mode-line faces."
  :group 'faces)

(defface greven-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path.")

(defface greven-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path.")

(defface greven-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line.")

(defface greven-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line.")

(defface greven-modeline-project-root-dir
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the project part of the mode-line buffer path.")

(defface greven-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line.")

(defface greven-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `greven-modeline--anzu', `greven-modeline--evil-substitute' and
`iedit'")

(defface greven-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'.")

(defface greven-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface greven-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'")

;; Bar
(defface greven-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window.")

(defface greven-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active.")

(defface greven-modeline-inactive-bar `((t (:background
                                          ,(face-foreground 'mode-line-inactive)
                                          :foreground
                                          ,(face-background 'mode-line-inactive))))
  "The face used for the left-most bar on the mode-line of an inactive window.")

(defface greven-modeline-evil-emacs-state '((t (:inherit greven-modeline-warning)))
  "Face for the Emacs state tag in evil state indicator.")

(defface greven-modeline-evil-insert-state'((t (:inherit greven-modeline-urgent)))
  "Face for the insert state tag in evil state indicator.")

(defface greven-modeline-evil-motion-state'((t :inherit greven-modeline-buffer-path))
  "Face for the motion state tag in evil state indicator.")

(defface greven-modeline-evil-normal-state'((t (:inherit greven-modeline-info)))
  "Face for the normal state tag in evil state indicator.")

(defface greven-modeline-evil-operator-state'((t (:inherit greven-modeline-buffer-path)))
  "Face for the operator state tag in evil state indicator.")

(defface greven-modeline-evil-visual-state'((t (:inherit greven-modeline-buffer-file)))
  "Face for the visual state tag in evil state indicator.")

(defface greven-modeline-evil-replace-state'((t (:inherit greven-modeline-buffer-modified)))
  "Face for the replace state tag in evil state indicator.")

;;
;; Modeline library
;;

(eval-and-compile
  (defvar greven-modeline-fn-alist ())
  (defvar greven-modeline-var-alist ()))

(defmacro greven-modeline-def-segment (name &rest body)
  "Defines a modeline segment NAME with BODY and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "greven-modeline-segment--%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "%s modeline segment" name))))
    (cond ((and (symbolp (car body))
                (not (cdr body)))
           (add-to-list 'greven-modeline-var-alist (cons name (car body)))
           `(add-to-list 'greven-modeline-var-alist (cons ',name ',(car body))))
          (t
           (add-to-list 'greven-modeline-fn-alist (cons name sym))
           `(progn
              (fset ',sym (lambda () ,docstring ,@body))
              (add-to-list 'greven-modeline-fn-alist (cons ',name ',sym))
              ,(unless (bound-and-true-p byte-compile-current-file)
                 `(let (byte-compile-warnings)
                    (byte-compile #',sym))))))))

(defun greven-modeline--prepare-segments (segments)
  "Prepare mode-line `SEGMENTS'."
  (let (forms it)
    (dolist (seg segments)
      (cond ((stringp seg)
             (push seg forms))
            ((symbolp seg)
             (cond ((setq it (cdr (assq seg greven-modeline-fn-alist)))
                    (push (list :eval (list it)) forms))
                   ((setq it (cdr (assq seg greven-modeline-var-alist)))
                    (push it forms))
                   ((error "%s is not a defined segment" seg))))
            ((error "%s is not a valid segment" seg))))
    (nreverse forms)))

(defun greven-modeline-def-modeline (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it.

  NAME is a symbol to identify it (used by `greven-modeline' for retrieval).
  LHS and RHS are lists of symbols of modeline segments defined with
  `greven-modeline-def-segment'.

  Example:
  (greven-modeline-def-modeline 'minimal
                              '(bar matches \" \" buffer-info)
                              '(media-info major-mode))
  (greven-modeline-set-modeline 'minimal t)"
  (let ((sym (intern (format "greven-modeline-format--%s" name)))
        (lhs-forms (greven-modeline--prepare-segments lhs))
        (rhs-forms (greven-modeline--prepare-segments rhs)))
    (defalias sym
      (lambda ()
        (let ((rhs-str (format-mode-line rhs-forms)))
          (list lhs-forms
                (propertize
                 " " 'display
                 `((space :align-to (- (+ right right-fringe right-margin)
                                       ,(+ 1 (string-width rhs-str))))))
                rhs-str)))
      (concat "Modeline:\n"
              (format "  %s\n  %s"
                      (prin1-to-string lhs)
                      (prin1-to-string rhs))))
    (unless (bound-and-true-p byte-compile-current-file)
      (let (byte-compile-warnings)
        (byte-compile sym)))))

(defun greven-modeline (key)
  "Return a mode-line configuration associated with KEY (a symbol).

  Throws an error if it doesn't exist."
  (let ((fn (intern-soft (format "greven-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun greven-modeline-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist.

  If DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let ((modeline (greven-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          (list "%e" modeline))))

(defun greven-modeline-project-root ()
  "Get the path to the root of your project.

  If STRICT-P, return nil if no project was found, otherwise return
  `default-directory'."
  (let (projectile-require-project-root)
    (projectile-project-root)))

;; Disable projectile mode-line segment
(setq projectile-dynamic-mode-line nil)

;;
;; Plugins
;;

(defun greven-modeline-eldoc (text)
  "Get eldoc TEXT for mode-line."
  (concat (when (display-graphic-p)
            (greven-modeline--make-xpm 'greven-modeline-eldoc-bar
                                     greven-modeline-height
                                     greven-modeline-bar-width))
          text))

;; Show eldoc in the mode-line with `eval-expression'
(defun greven-modeline--show-eldoc (input)
  "Display string INPUT in the mode-line next to minibuffer."
  (with-current-buffer (eldoc-current-buffer)
    (let* ((str              (and (stringp input) input))
           (mode-line-format (or (and str (or (greven-modeline-eldoc str) str))
                                 mode-line-format))
           mode-line-in-non-selected-windows)
      (force-mode-line-update)
      (sit-for eldoc-show-in-mode-line-delay))))
(setq eldoc-in-minibuffer-show-fn #'greven-modeline--show-eldoc)

(eldoc-in-minibuffer-mode 1)

;; anzu and evil-anzu expose current/total state that can be displayed in the
;; mode-line.
(defun greven-modeline-fix-anzu-count (positions here)
  "Calulate anzu counts via POSITIONS and HERE."
  (cl-loop for (start . end) in positions
           collect t into before
           when (and (>= here start) (<= here end))
           return (length before)
           finally return 0))

(advice-add #'anzu--where-is-here :override #'greven-modeline-fix-anzu-count)

;; Avoid anzu conflicts across buffers
;; (mapc #'make-variable-buffer-local
;;       '(anzu--total-matched anzu--current-position anzu--state
;;                             anzu--cached-count anzu--cached-positions anzu--last-command
;;                             anzu--last-isearch-string anzu--overflow-p))

;; Ensure anzu state is cleared when searches & iedit are done
;; (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
;; (add-hook '+evil-esc-hook #'anzu--reset-status t)
(add-hook 'iedit-mode-end-hook #'anzu--reset-status)

;; Keep `greven-modeline-current-window' up-to-date
(defvar greven-modeline-current-window (frame-selected-window))
(defun greven-modeline-set-selected-window (&rest _)
  "Set `greven-modeline-current-window' appropriately."
  (when-let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq greven-modeline-current-window win)
      (force-mode-line-update))))

(defun greven-modeline-unset-selected-window ()
  "Unset `greven-modeline-current-window' appropriately."
  (setq greven-modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'greven-modeline-set-selected-window)
(advice-add #'handle-switch-frame :after #'greven-modeline-set-selected-window)
(advice-add #'select-window :after #'greven-modeline-set-selected-window)
(with-no-warnings
  (cond ((not (boundp 'after-focus-change-function))
         (add-hook 'focus-in-hook  #'greven-modeline-set-selected-window)
         (add-hook 'focus-out-hook #'greven-modeline-unset-selected-window))
        ((defun greven-modeline-refresh-frame ()
           (setq greven-modeline-current-window nil)
           (cl-loop for frame in (frame-list)
                    if (eq (frame-focus-state frame) t)
                    return (setq greven-modeline-current-window (frame-selected-window frame)))
           (force-mode-line-update))
         (add-function :after after-focus-change-function #'greven-modeline-refresh-frame))))

;; Show version string for multi-version managers like rvm, rbenv, pyenv, etc.
(defvar-local greven-modeline-env-version nil)
(defvar-local greven-modeline-env-command nil)
(add-hook 'find-file-hook #'greven-modeline-update-env)
(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function #'greven-modeline-update-env)
    (add-hook 'focus-in-hook #'greven-modeline-update-env)))
(defun greven-modeline-update-env ()
  "Update environment info on mode-line."
  (when greven-modeline-env-command
    (let ((default-directory (or (greven-modeline-project-root) ""))
          (s (shell-command-to-string greven-modeline-env-command)))
      (setq greven-modeline-env-version (if (string-match "[ \t\n\r]+\\'" s)
                                          (replace-match "" t t s)
                                        s)))))


;;
;; Modeline helpers
;;

(defun greven-modeline-maybe-icon-octicon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-octicon args)))

(defun greven-modeline-maybe-icon-faicon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-faicon args)))

(defun greven-modeline-maybe-icon-material (&rest args)
  "Display material icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-material args)))

(defun greven-modeline--active ()
  "Whether is an active window."
  (eq (selected-window) greven-modeline-current-window))

(defun greven-modeline--make-xpm (face width height)
  "Create an XPM bitmap via FACE, WIDTH and HEIGHT. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or (face-background face nil t) "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun greven-modeline-buffer-file-name ()
  "Propertized variable `buffer-file-name' based on `greven-modeline-buffer-file-name-style'."
  (let ((buffer-file-name (or (buffer-file-name (buffer-base-buffer)) "")))
    (unless buffer-file-truename
      (setq buffer-file-truename (file-truename buffer-file-name)))
    (propertize
     (pcase greven-modeline-buffer-file-name-style
       (`truncate-upto-project
        (greven-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink))
       (`truncate-upto-root
        (greven-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename))
       (`truncate-all
        (greven-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename t))
       (`relative-to-project
        (greven-modeline--buffer-file-name-relative buffer-file-name buffer-file-truename))
       (`relative-from-project
        (greven-modeline--buffer-file-name-relative buffer-file-name buffer-file-truename 'include-project))
       (`file-name
        (propertize (file-name-nondirectory buffer-file-name)
                    'face
                    (let ((face (or (and (buffer-modified-p)
                                         'greven-modeline-buffer-modified)
                                    (and (greven-modeline--active)
                                         'greven-modeline-buffer-file))))
                      (when face `(:inherit ,face))))))
     'help-echo buffer-file-truename)))

(defun greven-modeline--buffer-file-name-truncate (file-path true-file-path &optional truncate-tail)
  "Propertized `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory true-file-path)))
        (active (greven-modeline--active)))
    (if (null dirs)
        (propertize "%b" 'face (if active 'greven-modeline-buffer-file))
      (let ((modified-faces (if (buffer-modified-p) 'greven-modeline-buffer-modified)))
        (let ((dirname (car dirs))
              (basename (cdr dirs))
              (dir-faces (or modified-faces (if active 'greven-modeline-project-root-dir)))
              (file-faces (or modified-faces (if active 'greven-modeline-buffer-file))))
          (concat (propertize (concat dirname
                                      (if truncate-tail (substring basename 0 1) basename)
                                      "/")
                              'face (if dir-faces `(:inherit ,dir-faces)))
                  (propertize (file-name-nondirectory file-path)
                              'face (if file-faces `(:inherit ,file-faces)))))))))


(defun greven-modeline--buffer-file-name-relative (_file-path true-file-path &optional include-project)
  "Propertized variable `buffer-file-name' showing directories relative to project's root only."
  (let ((root (greven-modeline-project-root))
        (active (greven-modeline--active)))
    (if (null root)
        (propertize "%b" 'face (if active 'greven-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'greven-modeline-buffer-modified))
             (relative-dirs (file-relative-name (file-name-directory true-file-path)
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'greven-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'greven-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory true-file-path)
                            'face (if file-faces `(:inherit ,file-faces))))))))

(defun greven-modeline--buffer-file-name (file-path _true-file-path &optional truncate-project-root-parent)
  "Propertized variable `buffer-file-name'.

If TRUNCATE-PROJECT-ROOT-PARENT is t space will be saved by truncating it down
fish-shell style.

Example:
  ~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el"
  (let* ((project-root (or (greven-modeline-project-root) ""))
         (file-name-split (shrink-path-file-mixed project-root
                                                  (file-name-directory file-path)
                                                  file-path))
         (active (greven-modeline--active)))
    (if (null file-name-split)
        (propertize "%b" 'face (if active 'greven-modeline-buffer-file))
      (pcase-let ((`(,root-path-parent ,project ,relative-path ,file-path) file-name-split))
        (let ((modified-faces (if (buffer-modified-p) 'greven-modeline-buffer-modified)))
          (let ((sp-faces       (or modified-faces (if active 'font-lock-comment-face)))
                (project-faces  (or modified-faces (if active 'font-lock-string-face)))
                (relative-faces (or modified-faces (if active 'greven-modeline-buffer-path)))
                (file-faces     (or modified-faces (if active 'greven-modeline-buffer-file))))
            (let ((sp-props       `(,@(if sp-faces       `(:inherit ,sp-faces))      ,@(if active '(:weight bold))))
                  (project-props  `(,@(if project-faces  `(:inherit ,project-faces)) ,@(if active '(:weight bold))))
                  (relative-props `(,@(if relative-faces `(:inherit ,relative-faces))))
                  (file-props     `(,@(if file-faces     `(:inherit ,file-faces)))))
              (concat (propertize (if truncate-project-root-parent
                                      root-path-parent
                                    (abbreviate-file-name project-root))
                                  'face sp-props)
                      (propertize (concat project "/") 'face project-props)
                      (if relative-path (propertize relative-path 'face relative-props))
                      (propertize file-path 'face file-props)))))))))


;;
;; buffer information
;;

(greven-modeline-def-segment buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (greven-modeline--active) 'greven-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (greven-modeline-maybe-icon-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

;;
(greven-modeline-def-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat (cond (buffer-read-only
                 (concat (greven-modeline-maybe-icon-octicon
                          "lock"
                          :face 'greven-modeline-warning
                          :v-adjust -0.05)
                         " "))
                ((buffer-modified-p)
                 (concat (greven-modeline-maybe-icon-faicon
                          "floppy-o"
                          :face 'greven-modeline-buffer-modified
                          :v-adjust -0.0575)
                         " "))
                ((and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
                 (concat (greven-modeline-maybe-icon-octicon
                          "circle-slash"
                          :face 'greven-modeline-urgent
                          :v-adjust -0.05)
                         " "))
                ((buffer-narrowed-p)
                 (concat (greven-modeline-maybe-icon-octicon
                          "fold"
                          :face 'greven-modeline-warning
                          :v-adjust -0.05)
                         " ")))
          (if buffer-file-name
              (greven-modeline-buffer-file-name)
            "%b")))

(greven-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (propertize
   "%b"
   'face (cond ((and buffer-file-name (buffer-modified-p))
                'greven-modeline-buffer-modified)
               ((greven-modeline--active) 'greven-modeline-buffer-file))))

;;
(greven-modeline-def-segment buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))


;;
;; major-mode
;;

(greven-modeline-def-segment major-mode
  "The major mode, including environment and text-scale info."
  (propertize
   (concat (format-mode-line
            `(:propertize ("" mode-name)
                          help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                          mouse-face mode-line-highlight
                          local-map ,mode-line-major-mode-keymap))
           (when greven-modeline-env-version
             (format " %s" greven-modeline-env-version))
           (and (boundp 'text-scale-mode-amount)
                (/= text-scale-mode-amount 0)
                (format
                 (if (> text-scale-mode-amount 0)
                     " (%+d)"
                   " (%-d)")
                 text-scale-mode-amount)))
   'face (if (greven-modeline--active) 'greven-modeline-buffer-major-mode)))


;;
;; process
;;

(greven-modeline-def-segment process
  "The process info."
  mode-line-process)

;;
;; vcs
;;

(defvar-local greven-modeline--vcs nil)
(defun greven-modeline--update-vcs (&rest _)
  "Update vsc state in mode-line."
  (setq greven-modeline--vcs
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (let ((face    'mode-line-inactive)
                  (active  (greven-modeline--active))
                  (all-the-icons-default-adjust -0.1))
              (concat (if (display-graphic-p) "  ")
                      (cond ((memq state '(edited added))
                             (if active (setq face 'greven-modeline-info))
                             (greven-modeline-maybe-icon-octicon
                              "git-compare"
                              :face face
                              :v-adjust -0.05))
                            ((eq state 'needs-merge)
                             (if active (setq face 'greven-modeline-info))
                             (greven-modeline-maybe-icon-octicon "git-merge" :face face))
                            ((eq state 'needs-update)
                             (if active (setq face 'greven-modeline-warning))
                             (greven-modeline-maybe-icon-octicon "arrow-down" :face face))
                            ((memq state '(removed conflict unregistered))
                             (if active (setq face 'greven-modeline-urgent))
                             (greven-modeline-maybe-icon-octicon "alert" :face face))
                            (t
                             (if active (setq face 'font-lock-doc-face))
                             (greven-modeline-maybe-icon-octicon
                              "git-branch"
                              :face face
                              :v-adjust -0.05)))
                      " "
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face (if active face))
                      " "))))))
(add-hook 'after-revert-hook #'greven-modeline--update-vcs)
(add-hook 'after-save-hook #'greven-modeline--update-vcs)
(add-hook 'find-file-hook #'greven-modeline--update-vcs t)
(advice-add #'select-window :after #'greven-modeline--update-vcs)

(greven-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (if (greven-modeline--active)
      greven-modeline--vcs
    ""))


;;
;; flycheck
;;

(defvar greven-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "Text style with icons in mode-line.")

(defun greven-modeline-icon (icon &optional text face voffset)
  "Displays an ICON with FACE, followed by TEXT.
Uses `all-the-icons-material' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (greven-modeline-maybe-icon-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text greven-modeline-vspc)))
          (if text (propertize text 'face face))
          (if vc-mode "  " " ")))

(defvar-local greven-modeline--flycheck nil)
(add-hook 'flycheck-status-changed-functions #'greven-modeline-update-flycheck-segment)
(add-hook 'flycheck-mode-hook #'greven-modeline-update-flycheck-segment)

(defun greven-modeline-update-flycheck-segment (&optional status)
  "Update flycheck segment via STATUS."
  (setq greven-modeline--flycheck
        (pcase status
          (`finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (greven-modeline-icon "do_not_disturb_alt"
                                                 (number-to-string sum)
                                                 (if .error 'greven-modeline-urgent 'greven-modeline-warning)
                                                 -0.25)))
                       (greven-modeline-icon "check" nil 'greven-modeline-info)))
          (`running     (greven-modeline-icon "access_time" nil 'font-lock-doc-face -0.25))
          (`no-checker  (greven-modeline-icon "sim_card_alert" "-" 'font-lock-doc-face))
          (`errored     (greven-modeline-icon "sim_card_alert" "Error" 'greven-modeline-urgent))
          (`interrupted (greven-modeline-icon "pause" "Interrupted" 'font-lock-doc-face)))))

(greven-modeline-def-segment flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (if (greven-modeline--active)
      greven-modeline--flycheck
    ""))


;;
;; selection-info
;;

(defsubst greven-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local greven-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline
segment.")

(greven-modeline-def-segment selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual)))
             (greven-modeline--active))
    (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (and (bound-and-true-p evil-visual-selection)
                                 (eq 'block evil-visual-selection)))
                        (let ((cols (abs (- (greven-modeline-column end)
                                            (greven-modeline-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((and (bound-and-true-p evil-visual-selection)
                             (eq evil-visual-selection 'line))
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       ((format "%dC" (- end beg))))
                 (when greven-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))))
       'face 'greven-modeline-highlight))))


;;
;; matches (anzu, evil-substitute, iedit, macro)
;;

(defun greven-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (greven-modeline--active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'greven-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'greven-modeline-panel)
              sep
              (greven-modeline-maybe-icon-octicon "triangle-right"
                                                :face 'greven-modeline-panel
                                                :v-adjust -0.05)
              sep))))

(defsubst greven-modeline--anzu ()
  "Show the match index and total number thereof.

Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (setq anzu-cons-mode-line-p nil)
  (when (and (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " total))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (greven-modeline--active) 'greven-modeline-panel))))

(defsubst greven-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and (bound-and-true-p evil-mode)
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (greven-modeline--active) 'greven-modeline-panel))))

(defun greven-modeline-themes--overlay-sort (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defsubst greven-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'greven-modeline-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (greven-modeline--active) 'greven-modeline-panel))))

(defsubst greven-modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (propertize
     (concat (car mc/mode-line)
             (eval (cadadr mc/mode-line))
             " ")
     'face (if (greven-modeline--active) 'greven-modeline-panel))))

(greven-modeline-def-segment matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (greven-modeline--macro-recording)
                      (greven-modeline--anzu)
                      (greven-modeline--evil-substitute)
                      (greven-modeline--iedit)
                      (greven-modeline--multiple-cursors))))
    (or (and (not (equal meta "")) meta)
        (if buffer-file-name " %I "))))


;;
;; media-info
;;

(greven-modeline-def-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  ;; TODO Include other information
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (image-size (image-get-display-property) :pixels)
           (format "  %dx%d  " width height)))))


;;
;; bar
;;

(defvar greven-modeline--bar-active nil)
(defvar greven-modeline--bar-inactive nil)
(greven-modeline-def-segment bar
  "The bar regulates the height of the mode-line in GUI Emacs.
  Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (if (greven-modeline--active)
          greven-modeline--bar-active
        greven-modeline--bar-inactive)
    ""))

(when (>= emacs-major-version 26)
  (add-variable-watcher
   'greven-modeline-height
   (lambda (_sym val op _where)
     (when (and (eq op 'set) (integerp val))
       (greven-modeline-refresh-bars greven-modeline-bar-width val))))

  (add-variable-watcher
   'greven-modeline-bar-width
   (lambda (_sym val op _where)
     (when (and (eq op 'set) (integerp val))
       (greven-modeline-refresh-bars val greven-modeline-height)))))

(add-hook 'after-setting-font-hook
          '(lambda ()
             (greven-modeline-refresh-bars)))


;;
;; window number
;;

(advice-add #'window-numbering-install-mode-line :override #'ignore)
(advice-add #'window-numbering-clear-mode-line :override #'ignore)

(greven-modeline-def-segment window-number
  (setq winum-auto-setup-mode-line nil)
  (let ((num (cond
              ((bound-and-true-p winum-mode)
               (winum-get-number-string))
              ((bound-and-true-p window-numbering-mode)
               (window-numbering-get-number-string))
              (t ""))))
    (if (< 0 (length num))
        (propertize (format " %s " num)
                    'face (if (greven-modeline--active)
                              'greven-modeline-buffer-major-mode))
      "")))


;;
;; workspace number
;;

(greven-modeline-def-segment workspace-number
  "The current workspace name or number. Requires `eyebrowse-mode' to be
enabled."
  (if (and (bound-and-true-p eyebrowse-mode)
           (< 1 (length (eyebrowse--get 'window-configs))))
      (let* ((num (eyebrowse--get 'current-slot))
             (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
             (str (if (and tag (< 0 (length tag)))
                      tag
                    (when num (int-to-string num)))))
        (propertize (format " %s " str) 'face
                    (if (greven-modeline--active) 'greven-modeline-buffer-major-mode)))
    ""))


;;
;; global
;;

(greven-modeline-def-segment global
  "For the time string and whatever uses global-mode-string."
  (if (< 0 (length global-mode-string))
      '(" " global-mode-string "  ")
    ""))


;;
;; position
;;

;; Be compatible with Emacs 25.
(defvar-local greven-modeline-column-zero-based
  (or (bound-and-true-p column-number-indicator-zero-based) t)
  "When non-nil, mode line displays column numbers zero-based.
See `column-number-indicator-zero-based'.")

(defvar-local greven-modeline-percent-position
  (or (bound-and-true-p mode-line-percent-position) '(-3 "%p"))
  "Specification of \"percentage offset\" of window through buffer.
See `mode-line-percent-position'.")

(setq-default mode-line-position
              '((line-number-mode
                 (column-number-mode
                  (greven-modeline-column-zero-based " %l:%c" " %l:%C")
                  " %l")
                 (column-number-mode (greven-modeline-column-zero-based " :%c" " :%C")))
                (if greven-modeline-percent-position (" " greven-modeline-percent-position))
                (:eval (when (or line-number-mode column-number-mode greven-modeline-percent-position) " "))))

(greven-modeline-def-segment buffer-position
  "The buffer position information."
  '(" " mode-line-position))


;;
;; evil-state
;;

(greven-modeline-def-segment evil-state
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (let ((tag (evil-state-property evil-state :tag t)))
      (propertize (s-trim-right tag) 'face
                  (if (greven-modeline--active)
                      (cond ((eq tag evil-normal-state-tag) 'greven-modeline-evil-normal-state)
                            ((eq tag evil-emacs-state-tag) 'greven-modeline-evil-emacs-state)
                            ((eq tag evil-insert-state-tag) 'greven-modeline-evil-insert-state)
                            ((eq tag evil-motion-state-tag) 'greven-modeline-evil-motion-state)
                            ((eq tag evil-visual-state-tag) 'greven-modeline-evil-visual-state)
                            ((eq tag evil-operator-state-tag) 'greven-modeline-evil-operator-state)
                            ((eq tag evil-replace-state-tag) 'greven-modeline-evil-replace-state)))))))


;;
;; input method
;;

(greven-modeline-def-segment input-method
  "The current input method."
  (cond
   (current-input-method
    (concat current-input-method-title "  "))
   ((and (bound-and-true-p evil-mode)
         (bound-and-true-p evil-input-method))
    (concat
     (nth 3 (assoc default-input-method input-method-alist))
     "  "))))

;;
;; Mode lines
;;

(greven-modeline-def-modeline 'main
                            '(bar workspace-number window-number evil-state matches " " buffer-info buffer-position " " selection-info)
                            '(global input-method buffer-encoding major-mode process vcs flycheck))

(greven-modeline-def-modeline 'minimal
                            '(bar matches " " buffer-info)
                            '(media-info major-mode))

(greven-modeline-def-modeline 'special
                            '(bar window-number evil-state matches " " buffer-info-simple buffer-position " " selection-info)
                            '(global input-method buffer-encoding major-mode process flycheck))

(greven-modeline-def-modeline 'project
                            '(bar window-number buffer-default-directory)
                            '(global major-mode))

(greven-modeline-def-modeline 'media
                            '(bar window-number " %b  ")
                            '(global media-info major-mode))

;;
;; Hooks
;;

(defun greven-modeline-refresh-bars (&optional width height)
  "Refresh mode-line bars with `WIDTH' and `HEIGHT'."
  (setq greven-modeline--bar-active
        (greven-modeline--make-xpm 'greven-modeline-bar
                                 (or width greven-modeline-bar-width)
                                 (max (or height greven-modeline-height)
                                      (frame-char-height)))
        greven-modeline--bar-inactive
        (greven-modeline--make-xpm 'greven-modeline-inactive-bar
                                 (or width greven-modeline-bar-width)
                                 (max (or height greven-modeline-height)
                                      (frame-char-height)))))

;;;###autoload
(defun greven-modeline-init ()
  "Initialize greven mode-line."
  ;; Create bars
  (greven-modeline-refresh-bars)
  (unless after-init-time
    ;; These buffers are already created and don't get modelines. For the love
    ;; of Emacs, someone give the man a modeline!
    (dolist (bname '("*scratch*" "*Messages*"))
      (with-current-buffer bname
        (greven-modeline-set-modeline 'main)))))

;;;###autoload
(defun greven-modeline-set-special-modeline ()
  "Set sepcial mode-line."
  (greven-modeline-set-modeline 'special))

;;;###autoload
(defun greven-modeline-set-media-modeline ()
  "Set media mode-line."
  (greven-modeline-set-modeline 'media))

;;;###autoload
(defun greven-modeline-set-project-modeline ()
  "Set project mode-line."
  (greven-modeline-set-modeline 'project))

;;
;; Bootstrap
;;

(greven-modeline-set-modeline 'main t) ; set default modeline

(add-hook 'image-mode-hook #'greven-modeline-set-media-modeline)
(add-hook 'org-src-mode-hook #'greven-modeline-set-special-modeline)
(add-hook 'circe-mode-hook #'greven-modeline-set-special-modeline)

;; Versions, support Python, Ruby, Perl and Golang, etc.
(add-hook 'python-mode-hook
          (lambda ()
            (when (and (executable-find greven-modeline-python-executable) (executable-find "cut") (executable-find "sed"))
              (setq greven-modeline-env-command (concat greven-modeline-python-executable " --version 2>&1 | cut -d' ' -f2 | sed -n '1p'")))))
(add-hook 'ruby-mode-hook
          (lambda ()
            (when (and (executable-find "ruby") (executable-find "cut") (executable-find "sed"))
              (setq greven-modeline-env-command "ruby --version 2>&1 | cut -d' ' -f2 | sed -n '1p'"))))
(add-hook 'perl-mode-hook
          (lambda ()
            (when (and (executable-find "perl") (executable-find "cut") (executable-find "tr") (executable-find "sed"))
              (setq greven-modeline-env-command "perl --version 2>&1 | cut -d'(' -f2 | cut -d')' -f1 | tr -d 'v' | sed -n '2p'"))))
(add-hook 'go-mode-hook
          (lambda ()
            (when (and (executable-find "go") (executable-find "cut") (executable-find "tr") (executable-find "sed"))
              (setq greven-modeline-env-command "go version 2>&1 | cut -d' ' -f3 | tr -d 'go' | sed -n '1p'"))))
(add-hook 'elixir-mode-hook
          (lambda ()
            (when (and (executable-find "iex") (executable-find "cut") (executable-find "sed"))
              (setq greven-modeline-env-command "iex --version 2>&1 | cut -d' ' -f2 | sed -n '1p'"))))


;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar greven-modeline-remap-face-cookie nil)
(defun greven-modeline-focus ()
  "Focus mode-line."
  (when greven-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative greven-modeline-remap-face-cookie)))
(defun greven-modeline-unfocus ()
  "Unfocus mode-line."
  (setq greven-modeline-remap-face-cookie (face-remap-add-relative 'mode-line 'mode-line-inactive)))

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (defun greven-modeline-focus-change ()
          (if (frame-focus-state)
              (greven-modeline-focus)
            (greven-modeline-unfocus)))
        (add-function :after after-focus-change-function #'greven-modeline-focus-change))
    (progn
      (add-hook 'focus-in-hook #'greven-modeline-focus)
      (add-hook 'focus-out-hook #'greven-modeline-unfocus))))

(provide 'greven-modeline)

;;; greven-modeline.el ends here
