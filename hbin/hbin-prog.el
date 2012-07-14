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

(require 'rainbow-delimiters)          ; highlights parentheses, brackets, and braces according to their depth.

(defun hbin-prog-hook ()
  (turn-on-hs-mode)
  (turn-on-linum-mode)
  (turn-on-watchwords)
  (turn-on-autopair-mode)
  (turn-on-projectile-mode)
  (turn-on-rainbow-delimiters-mode))

(defvar programming-modes '(c-mode-common-hook
                            python-mode-hook
                            java-mode-hook
                            lua-mode-hook
                            js-mode-hook js2-mode-hook
                            css-mode-hook sass-mode-hook
                            ruby-mode-hook rhtml-mode-hook
                            yaml-mode-hook coffee-mode-hook
                            haml-mode-hook slim-mode-hook
                            rspec-mode-hook
                            lisp-interaction-mode-hook
                            emacs-lisp-mode-hook))

(mapc (lambda (mode-hook) (add-hook mode-hook 'hbin-prog-hook)) programming-modes)

;; Automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  (indent-region beg end nil))

(defadvice yank (after yank-indent activate)
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode programming-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode programming-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

;;;;;;;  Enhanced programming languages

(require 'prog-c)
(require 'prog-js)
(require 'prog-lua)
(require 'prog-web)
(require 'prog-lisp)
(require 'prog-ruby)
(require 'prog-rails)
(require 'prog-python)
(require 'prog-clojure)
(require 'prog-markdown)

(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-prog)

;;;;;; hbin-prog.el END
