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

(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

;; Explicitly set key of snippets
(setq yas/ignore-filenames-as-triggers t)

;; Personal snippets
(setq-default yas/snippet-dirs (concat user-emacs-directory "snippets"))

;; Load the snippets
(yas/load-directory yas/root-directory)

;; When entering rinari-minor-mode, consider also the snippets in the
;; snippet table "rails-mode"
(add-hook 'rinari-minor-mode-hook
          #'(lambda ()
              (setq yas/mode-symbol 'rails-mode)))

(yas/initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-yas)

;;;;; misc-yas.el END
