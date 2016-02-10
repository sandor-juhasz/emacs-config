;;;
;;; Sandor Juhasz' init.el
;;;
;;; (C) 2015 Sandor Juhasz
;;;
;;; These are my Emacs settings I use for Clojure development and
;;; various other tasks using Emacs. With this config my Emacs
;;; installation is set up as a single editor running in server
;;; mode.
;;;
;;; Personally I use emacs to1
;;;    - Develop clojure projects
;;;    - Author documentation and blog posts in Org mode.
;;;
;;; To fully take advantage of this configuration, please install the
;;; following external applications too.
;;;    - Java SE 8
;;;    - wget
;;;    - leiningen
;;;    - git
;;; Please see the setup.org file for step-by-step installation steps
;;; of these external tools.
;;;

;;
;; General emacs settings to integrate the tool into my Windows
;; toolchain
;;

(add-to-list 'load-path "~/.emacs.d/lisp/sanyi")
(add-to-list 'load-path "~/.emacs.d/lisp/org/")

(server-start)
(setq inhibit-startup-message t) 
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8) ; This is required to get rid of encoding issues
                              ; encountered during package management.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 113 :width normal)))))

;;
;; Package management and auto-installation
;;
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)
(setq package-list '(cider
		     company
		     flx
		     flx-ido
		     paredit
		     plantuml-mode
		     rainbow-delimiters
		     elisp-slime-nav
		     idomenu
		     imenu-anywhere
		     use-package))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;
;;; Ido mode settings
;;;
(ido-mode t)
(require 'flx-ido)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
;; Other options include
;; - Not asking for conformation when creating a new buffer
;; - Specifying extension ordering for ido
;; - Ignoring files

;;;
;;; imenu settings
;;;
(global-set-key (kbd "C-c i") 'idomenu)
(global-set-key (kbd "C-c C-i") 'imenu-anywhere)

;;;
;;; Emacs lisp mode settings
;;;
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

;; Shortcuts to easily open declarations of various elisp objects
;;
;; http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
;;
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;;;
;;; Cider settings
;;;
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'company-mode)

(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'remove-dos-eol)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(setq cider-prompt-save-file-on-load   nil) ; Does not prompt for buffer save when loading file with C-c C-k

;;;
;;; Org-mode settings
;;;

(require 'ox-reveal)
(setq org-reveal-root "file:///c:/Dev/Tools/revealjs")

;; The following lines are used to auto-refresh generated images in an org file
(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(global-set-key "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-log-done 'time ; When enabled, closing a TODO will insert a CLOSED timestamp.
      org-confirm-babel-evaluate nil
      org-plantuml-jar-path "c:/Dev/Tools/PlantUML/plantuml.jar"
      org-babel-clojure-backend 'cider
      org-babel-sh-command "bash"
      org-agenda-files (list "~/org/opensource.org"
			     "~/org/network.org"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (clojure . t)
   (plantuml . t)
   (dot . t)))



;;;
;;; Spell checker settings
;;;
(setq ispell-program-name (executable-find "hunspell")
      ispell-local-dictionary "en_US"
      ispell-skip-html t
      ispell-local-dictionary-alist
      '((nil "\[\[:alpha:\]\]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
	("american" "\[\[:alpha:\]\]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
	("en_US" "\[\[:alpha:\]\]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))
(setq ispell-dictionary "en_US")
(require 'ispell)




;;;
;;; Plantuml-mode settings
;;;
(setq plantuml-jar-path "c:/Dev/Tools/PlantUML/plantuml.jar")

(require 'use-package)

(use-package magit
  :ensure t
  :bind ("C-c m m" . magit-status)
  :config (setenv "GIT_ASKPASS" "git-gui--askpass"))

(use-package powerline ; Powerline (milkypostman version) from MELPA
  :ensure t
  :config
  (powerline-default-theme))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package yaml-mode
  :ensure t
  :config
   (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
   (add-hook 'yaml-mode-hook  ;; To ensure auto-indentation when pressing ENTER
    '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;;;
;;; Other key mappings and functions
;;;
(defun reformat-xml ()
  "Reformats the XML file in the buffer using "
  (interactive)
  (mark-whole-buffer)
  (shell-command-on-region (point-min) (point-max) "xml format" t t))

(defvar sandors-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'base64-encode-region)
    (define-key map (kbd "d") #'base64-decode-region)
    (define-key map (kbd "x") #'reformat-xml)
    map))

(global-set-key (kbd "C-c s") sandors-keymap)

