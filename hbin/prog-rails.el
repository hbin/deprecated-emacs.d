;; prog-rails.el --- Enhance Rails programming
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

;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")

;; Rake files are ruby too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;;;###autoload
(progn
  (require 'haml-mode)
  (require 'sass-mode)
  (require 'slim-mode)
  (require 'yaml-mode)

  (require 'coffee-mode)
  (custom-set-variables '(coffee-tab-width 2))

  ;; Setting Scss
  (require 'scss-mode)
  (add-hook 'scss-mode-hook
            (lambda ()
              (rainbow-mode)
              (setq scss-compile-at-save nil)
              (define-key scss-mode-map (kbd "C-c C-c") 'whole-line-or-region-comment-dwim-2)))

  ;; Minor modes in rhtml
  (require 'rhtml-mode)
  (add-hook 'rhtml-mode-hook
            (lambda ()
              (auto-complete-mode t)
              (abbrev-mode -1)          ; Don't need abbrev
              (define-key rhtml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
              (define-key rhtml-mode-map (kbd "C-c C-c") 'whole-line-or-region-comment-dwim-2)))

  ;; Awesome plugin for programming Rails App.
  (require 'rinari)
  (setq rinari-tags-file-name "TAGS")
  (add-hook 'rinari-minor-mode-hook     ; TODO: rinari-web-server-restart binding to z
            (lambda ()
              (define-key rinari-minor-mode-map (kbd "C-.") 'insert-arrow))))

(provide 'prog-rails)
