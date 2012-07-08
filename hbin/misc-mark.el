;; misc-mark.el --- Configuration for highlight and mark symbols.
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

;; Amazing expand-region
(require 'expand-region)
(global-set-key (kbd "M-h") 'er/expand-region)

;; Highlight symbols with different colors.
(require 'highlight-symbol)
(global-set-key (kbd "M-m") 'highlight-symbol-at-point)
(global-set-key (kbd "M-M") 'highlight-symbol-remove-all)
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)
(global-set-key (kbd "M-N") 'highlight-symbol-next-in-defun)
(global-set-key (kbd "M-P") 'highlight-symbol-prev-in-defun)

;; Rectangle operate
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Mark several regions at once
(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;; Mark pairs of html tags
(require 'rename-sgml-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-mark)

;;;;; misc-mark.el END
