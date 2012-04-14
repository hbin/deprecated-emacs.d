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
(require 'idle-highlight-mode)         ; 高亮 Buffer 中所有与光标所在单词相同的内容

(add-hook 'prog-mode-hook 'turn-on-watchwords)
(add-hook 'prog-mode-hook 'turn-on-projectile-mode)
;; (add-hook 'prog-mode-hook 'turn-on-idle-highlight-mode)
(add-hook 'prog-mode-hook 'turn-on-rainbow-delimiters-mode)

;;;;;;;  Enhanced programming languages

(require 'prog-c)
(require 'prog-web)
(require 'prog-lisp)
(require 'prog-ruby)
(require 'prog-rails)
(require 'prog-python)
(require 'prog-clojure)
(require 'prog-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-prog)

;;;;;; hbin-prog.el END
