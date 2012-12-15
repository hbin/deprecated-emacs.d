;; prog-lisp.el --- Enhance Lisp programming
;;
;; Copyright (C) 2012 Huang Bin
;;
;; Author: Huang Bin <embrace.hbin@gmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;###autoload
(defun hbin-lisp-mode-init ()
  (autopair-mode -1)
  (paredit-mode 1)
  (eldoc-mode 1)
  (local-set-key (kbd "<return>") 'newline-and-indent))

(add-hook 'emacs-lisp-mode-hook 'hbin-lisp-mode-init)

(provide 'prog-lisp)
