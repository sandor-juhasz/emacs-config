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
(setq ring-bell-function 'ignore)

;;
;; Package management and auto-installation
;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)
(package-refresh-contents)
(setq package-list '(cider
		     company
		     magit
		     paredit
		     rainbow-delimiters
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
