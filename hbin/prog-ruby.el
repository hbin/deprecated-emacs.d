;; prog-ruby.el --- Enhance Ruby programming
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

;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")

;; Rake files are ruby too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;;; RVM
(require 'rvm)
(rvm-use-default)

;;; Ruby tools
(require 'yari)
(require 'ruby-tools)
(define-key 'help-command "R" 'yari)

;;; Ruby block highlight
(require 'ruby-block)
(custom-set-variables
 '(ruby-block-delay 0))

;;; Rspec {{{
(require 'rspec-mode)

;; workaround for ZSH issues.
(defadvice rspec-compile (around rspec-compile-around)
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)
;;; }}}

;;; Rsense {{{
(setq rsense-home (substitute-in-file-name "$RSENSE_HOME"))
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense nil 'noerror)
(if (and (not (eq rsense-home "$RSENSE_HOME"))
         (featurep 'rsense))
    (progn
      (add-hook 'ruby-mode-hook
                (lambda ()
                  (add-to-list 'ac-sources 'ac-source-rsense-constant)
                  (add-to-list 'ac-sources 'ac-source-rsense-method)))
      (add-hook 'kill-emacs-hook 'rsense-exit)))
;;; }}}

;;; Insert arrow {{{
(defun insert-arrow ()
  "Insert arrow and put cursor at the right position."
  (interactive)
  (delete-horizontal-space t)
  (insert " => "))
(global-set-key (kbd "C-.") 'insert-arrow)
;;; }}}

;;; Ruby mode {{{
(defun hbin-ruby-mode-setup ()
  ;; Font lock for new hash style
  (font-lock-add-keywords
   'ruby-mode
   '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face)))))

(defun hbin-ruby-mode-init ()
  (local-set-key (kbd "<return>") 'newline-and-indent)

  ;; Words prefixed with $ are global variables,
  ;; prefixed with @ are instance variables.
  (modify-syntax-entry ?$ "w")
  (modify-syntax-entry ?@ "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?: ".")

  (flycheck-mode 1)
  (ruby-block-mode 1))

(eval-after-load 'ruby-mode '(hbin-ruby-mode-setup))
(add-hook 'ruby-mode-hook 'hbin-ruby-mode-init)
;;; }}}

;;; Rinari {{{
;; Awesome plugin for programming Rails App.
(require 'rinari)
(defun hbin-rinari-minor-mode-init ()
  (setq yas-extra-modes (cons 'rails-mode yas-extra-modes)))
(add-hook 'rinari-minor-mode-hook 'hbin-rinari-minor-mode-init)
(global-rinari-mode)
;;; }}}

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prog-ruby)
