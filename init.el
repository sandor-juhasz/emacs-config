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

;;
;; Package management and auto-installation
;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)
