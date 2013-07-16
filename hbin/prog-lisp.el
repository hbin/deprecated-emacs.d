;; prog-lisp.el --- Enhance Lisp programming
;;
;; Copyright (C) 2012-2013 Huang Bin
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

(require'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(defun hbin-lisp-mode-init ()
  (autopair-mode -1)
  (paredit-mode 1)
  (eldoc-mode 1)
  (local-set-key (kbd "M-s") hbin-map)
  (local-set-key (kbd "M-\\") 'paredit-splice-sexp)
  (local-set-key (kbd "<return>") 'newline-and-indent))

(add-hook 'lisp-mode-hook 'hbin-lisp-mode-init)
(add-hook 'emacs-lisp-mode-hook 'hbin-lisp-mode-init)

(provide 'prog-lisp)
