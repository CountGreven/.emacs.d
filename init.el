;;; init.el ---- Grevens init.el file

;;; Commentary:

;;;;
;; Packages
;;;;

;;; Code:

;; Define package repositories

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.

(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.

(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:

(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit
    git-gutter+
    git-timemachine
    
    ;; puppet-mode and flymake-puppet to edit and lint puppet manifests for work
    puppet-mode
    flymake-puppet
    
    ;; pdf-tools to enhance pdf capabilities
    pdf-tools

    ;; Org-bullets to make our .org files a bit prettier
    org-bullets

    ;;All-the-icons to prettify our modeline etc.
    all-the-icons

    ;;Visible mark mode so we can see where we put the mark
    visible-mark
    ;; Company mode for autocompletion
    company

    ;;Yasnippet, to have automatic snippet completion
    yasnippet
    yasnippet-snippets
    auto-yasnippet

    ;;Flycheck mode for syntax highlighting and linting
    flycheck
    flycheck-yamllint
    flycheck-clojure))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file

(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.

(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/customizations/modeline")
;; Set file for emacs built-in customize settings.
;; This way we can byte compile our init.el and still have customize
;; make changes that will be read at startup.

(setq custom-file "~/.emacs.d/customizations/customize.el")
(load "customize")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables

(load "shell-integration")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

(load "navigation")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements

(load "ui")
(load "greven-modeline")
(load "modeline")

;; These customizations make editing a bit nicer.

(load "editing")

;; Hard-to-categorize customizations

(load "misc")

;; For editing lisps

(load "elisp-editing")

;; Enhancements to org-mode

(load "custom-org")

;; Langauage-specific

(load "setup-clojure")

(load "setup-js")

;; OS specific
(if (eq system-type 'darwin)
    (load "osx"))

;; Footer

(provide 'init)
;;; init.el ends here
