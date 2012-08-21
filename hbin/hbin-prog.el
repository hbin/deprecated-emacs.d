;; hbin-prog.el --- Feel fly when I program.
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
(progn
  (require 'rvm)
  (require 'rainbow-mode)
  (require 'zencoding-mode)

  (rvm-use-default)
  (setq feature-use-rvm t)

  (defvar hbin-syntax-table
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?_ "w" table)
      (modify-syntax-entry ?- "w" table)
      table))

  (defun prog-common-setting ()
    "Common settings for programming."
    (turn-on-watchwords)
    (turn-on-autopair-mode)
    (turn-on-textmate-mode)
    (turn-on-rainbow-delimiters-mode)
    (set-syntax-table hbin-syntax-table)
    (local-set-key (kbd "C-M-h") 'backward-kill-word)
    (local-set-key (kbd "C-c C-c") 'whole-line-or-region-comment-dwim-2))

  (add-hook 'prog-mode-hook 'prog-common-setting))

;;;  Enhanced programming languages
(require 'prog-js)
(require 'prog-lua)
(require 'prog-web)
(require 'prog-lisp)
(require 'prog-scss)
(require 'prog-haml)
(require 'prog-yaml)
(require 'prog-slim)
(require 'prog-ruby)
(require 'prog-rails)
(require 'prog-coffee)
(require 'prog-python)
(require 'prog-feature)

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-prog)

;;;;;; hbin-prog.el END
