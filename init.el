;;;
;;; Sandor Juhasz' init.el
;;;
;;; These are my Emacs settings I use for Clojure development and
;;; various other tasks using Emacs.
;;;

;;
;; General emacs settings to integrate the tool into my Windows
;; toolchain
;;
(server-start)
(setq inhibit-startup-message t) 

;;
;; Package management and auto-installation
;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)
(package-refresh-contents)
(setq package-list '(magit
		     solarized-theme))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;
;;; Theme settings
;;;
(load-theme 'solarized-dark t)

;;;
;;; Magit settings
;;;
(setq magit-last-seen-setup-instructions "1.4.0")
