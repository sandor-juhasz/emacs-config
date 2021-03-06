;;; sanyi-demo.el --- Demo module to pilot the loading of the modules.

;; Copyright (C) 2016 Sandor Juhasz

;; Author: Sandor Juhasz <sandor.juhasz.1983@gmail.com>
;; URL: https://github.com/sandor-juhasz
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Hello world demo
;;
;;; Code:

(defun sanyi-demo-hello ()
  "Returns a hello world message."
  (interactive)
  (message "Hello, World!")
  )

(global-set-key (kbd "C-c s h") 'sanyi-demo-hello)

(provide 'sanyi-demo)

;;; sanyi-demo.el ends here
