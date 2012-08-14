;; prog-ruby.el --- Enhance Ruby programming
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
  (require 'yari)

  (rvm-use-default)

  (eval-after-load 'ruby-mode
    '(progn
       ;; Ruby key Binding
       (define-key 'help-command "R" 'yari)
       (define-key ruby-mode-map (kbd "C-.") 'insert-arrow)
       (define-key ruby-mode-map (kbd "C-c C-c") 'whole-line-or-region-comment-dwim-2))))

;;;###autoload
(defun insert-arrow ()
  "Insert arrow and put cursor at the right position."
  (interactive)
  (delete-horizontal-space t)
  (insert " => "))

;; hs-minor-mode for ruby mode
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\|{\\)" "\\(end\\|end\\|end\\|end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

(provide 'prog-ruby)
