;; misc-ibuffer.el --- Configuration for Ibuffer
;;
;; Copyright (C) 2012-2013 Huang Bin
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

(custom-set-variables
 '(ibuffer-always-show-last-buffer :nomini)
 '(ibuffer-default-shrink-to-minimum-size t)
 '(ibuffer-jump-offer-only-visible-buffers nil)
 '(ibuffer-show-empty-filter-groups nil))

(defun hbin-ibuffer-mode-setup ()
  (define-key ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file)
  (define-key ibuffer-mode-map (kbd "M-o") 'other-window)

  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Dired" (mode . dired-mode))
                 ("Code"
                  (or
                   (mode . c-mode)
                   (mode . clojure-mode)
                   (mode . java-mode)))
                 ("Lisp"
                  (or
                   (mode . lisp-mode)
                   (mode . emacs-lisp-mode)))
                 ("Ruby"
                  (or
                   (mode . ruby-mode)))
                 ("Python"
                  (or
                   (mode . python-mode)))
                 ("Web"
                  (or
                   (mode . css-mode)
                   (mode . js-mode)
                   (mode . js2-mode)
                   (mode . javascript-mode)
                   (mode . coffee-mode)
                   (mode . haml-mode)
                   (mode . slim-mode)
                   (mode . scss-mode)
                   (mode . yaml-mode)
                   (mode . html-mode)
                   (mode . rhtml-mode)
                   (mode . nxhtml-mode)))
                 ("Docs"
                  (or
                   (mode . org-mode)
                   (mode . text-mode)
                   (mode . markdown-mode)))
                 ("Terminals"
                  (or
                   (mode . shell-mode)
                   (mode . eshell-mode)))
                 ("System"
                  (or
                   (mode . help-mode)
                   (mode . completion-list-mode)
                   (mode . apropos-mode)
                   (name . "^\\*.*\\*$")))
                 )))))

(defun hbin-ibuffer-mode-init ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (hl-line-mode))

(eval-after-load "ibuffer" '(hbin-ibuffer-mode-setup))
(add-hook 'ibuffer-mode-hook 'hbin-ibuffer-mode-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-ibuffer)

;;;;;;; misc-ibuffer.el END
