;; misc-navigation.el --- fast navigation
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

;; File tree browser
(require 'dirtree)

;; Ack is a tool like grep, optimized for programmers
(require 'full-ack)
(setq ack-executable (executable-find "ack-grep"))

;; A fancy and quickly cursor location minor mode
(require 'ace-jump-mode)
(global-set-key (kbd "M-i") 'ace-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-navigation)

;;;;;; misc-navigation.el END
