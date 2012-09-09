;; misc-yas.el --- Configuration for Yasnippet
;;
;; Copyright (C) 2012 Huang Bin
;;
;; Author: Huang Bin <embrace.hbin@gmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary

;; Yasnippet - https://github.com/capitaomorte/yasnippet

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

(require 'yasnippet)
(require 'dropdown-list)

;; Explicitly setting trigger key to "TAB", rather than <tab>
(setq yas-trigger-key "TAB")

;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

;; ido-stype candidates
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

;; Set personal snippets as default
(setq yas-snippet-dirs (concat user-emacs-directory "snippets"))

(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-yas)

;;;;; misc-yas.el END
