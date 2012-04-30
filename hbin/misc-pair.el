;; misc-pair.el --- Configuration for Pairs
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

;; smart indenting and pairing for all
;; (when (>= emacs-major-version 24)
;;   (electric-pair-mode t)
;;   (electric-indent-mode t)
;;   (electric-layout-mode t))

;; Auto Pair
(require 'autopair)
(setq autopair-blink nil)               ; paren-blinking annoying indeed

(if (>= emacs-major-version 24)
    (progn
      (add-hook 'prog-mode-hook 'toggle-auto-pair-mode)
      (add-hook 'lisp-interaction-mode-hook 'toggle-auto-pair-mode))
  (progn
    (autopair-global-mode)
    (add-hook 'text-mode-hook 'toggle-auto-pair-mode)
    (add-hook 'message-mode-hook 'toggle-auto-pair-mode)))

;; ParEdit 自动补全括号(更强大)
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-pair)

;;;;;; misc-pair.el END
