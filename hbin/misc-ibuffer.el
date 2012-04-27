;; misc-ibuffer.el --- Configuration for Ibuffer
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

(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Documents"
                (or
                 (mode . org-mode)
                 (mode . markdown-mode)))
               ("Directories"
                (mode . dired-mode))
               ("Programming"
                (or
                 (mode . c-mode)
                 (mode . java-mode)
                 (mode . python-mode)
                 (mode . clojure-mode)
                 (mode . js-mode)
                 (mode . js2-mode)
                 (mode . javascript-mode)
                 (mode . html-mode)
                 (mode . nxml-mode)
                 (mode . lisp-mode)
                 (mode . emacs-lisp-mode)))
               ("Ruby on Rails"
                (or
                 (mode . ruby-mode)
                 (mode . yaml-mode)
                 (mode . haml-mode)
                 (mode . slim-mode)
                 (mode . rhtml-mode)
                 (mode . nxhtml-mode)
                 (mode . coffee-mode)))
               ("Terminals"
                (or
                 (mode . shell-mode)
                 (mode . eshell-mode)))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; override default find-file in Ibuffer mode
(define-key ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-ibuffer)

;;;;;;; misc-ibuffer.el END