;; prog-web.el --- Enhance HTML & CSS
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

(custom-set-variables
 '(js-indent-level 2)
 '(css-indent-offset 2)
 '(coffee-tab-width 2)
 '(zencoding-indentation 2))

;;; Colorize color names in buffers
(require 'rainbow-mode)

;; Zencoding {{{
(require 'zencoding-mode)
(custom-set-variables '(zencoding-indentation 2))
(define-key zencoding-mode-keymap (kbd "C-j") nil)
(define-key zencoding-mode-keymap (kbd "<C-return>") nil)
(define-key zencoding-mode-keymap (kbd "C-c C-j") 'zencoding-expand-line)
;;; }}}

;;; HTML {{{
(defun hbin-html-mode-init ()
  (rainbow-mode 1)
  (auto-complete-mode 1)
  (zencoding-mode 1)
  (hbin-prog-mode-init))
(add-hook 'sgml-mode-hook 'hbin-html-mode-init)
;;; }}}

;;; CSS {{{
(defun hbin-css-mode-init ()
  (rainbow-mode 1)
  (hbin-prog-mode-init))
(add-hook 'css-mode-hook 'hbin-css-mode-init)
;;; }}}

;;; Javascript {{{
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.erb$" . js-mode))

(require 'js2-mode)
(defun hbin-js-mode-init ()
  (hbin-prog-mode-init))
(add-hook 'js2-mode-hook 'hbin-js-mode-init)
;;; }}}

;;; Rhtml-mode {{{
(require 'rhtml-mode)
(defun hbin-rhtml-mode-init ()
  (ruby-tools-mode 1))

(eval-after-load "rhtml-mode"
  '(add-to-list
    'rhtml-in-erb-keywords
    '("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" . (1 font-lock-constant-face prepend))))
(add-hook 'rhtml-mode-hook 'hbin-rhtml-mode-init)
;;; }}}

;;; Haml-mode {{{
(require 'haml-mode)
;;; }}}

;;; Slim-mode {{{
(require 'slim-mode)
(defun hbin-slim-mode-init ()
  (hbin-prog-mode-init))
(add-hook 'slim-mode-hook 'hbin-slim-mode-init)
;;; }}}

;;; Mustache-mode {{{
(require 'mustache-mode)
(defun hbin-mustache-mode-init ()
  (hbin-prog-mode-init))
(add-hook 'mustache-mode-hook 'hbin-mustache-mode-init)
;;; }}}

;;; Scss-mode {{{
(require 'scss-mode)
(defun hbin-scss-mode-init ()
  (rainbow-mode 1)
  (auto-complete-mode 1)
  (setq scss-compile-at-save nil)
  (hbin-prog-mode-init))
(add-hook 'scss-mode-hook 'hbin-scss-mode-init)
;;; }}}

;;; Yaml {{{
(require 'yaml-mode)
(defun hbin-yaml-mode-init ()
  (hbin-prog-mode-init))
(add-hook 'yaml-mode-hook 'hbin-yaml-mode-init)
;;; }}}

;;; Coffee {{{
(require 'coffee-mode)
(defun hbin-coffee-mode-init ()
  (hbin-prog-mode-init))
(add-hook 'coffee-mode-hook 'hbin-coffee-mode-init)
;;; }}}

;;;;;;;;;;;;;;;;;;;;;

(provide 'prog-web)
