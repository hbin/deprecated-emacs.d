;; prog-ruby.el --- Enhance Ruby programming
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

;;;###autoload
(progn
  ;; RVM
  (require 'rvm)
  (rvm-use-default)

  ;; Ruby tools
  (require 'yari)
  (require 'ruby-tools)

  ;; Setting for Ruby block mode
  (require 'ruby-block)
  (setq ruby-block-delay 0)
  (setq ruby-block-highlight-toggle t)
  (ruby-block-mode t)

  ;; Rspec and its workaround for ZSH issues.
  (require 'rspec-mode)
  (defadvice rspec-compile (around rspec-compile-around)
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))
  (ad-activate 'rspec-compile)

  ;; Setting for Rsense
  (setq rsense-home (substitute-in-file-name "$RSENSE_HOME"))
  (add-to-list 'load-path (concat rsense-home "/etc"))
  (require 'rsense nil 'noerror)
  (if (featurep 'rsense)
      (progn
        (add-hook 'ruby-mode-hook
                  (lambda ()
                    (add-to-list 'ac-sources 'ac-source-rsense-constant)
                    (add-to-list 'ac-sources 'ac-source-rsense-method)))
        (add-hook 'kill-emacs-hook
                  (lambda ()
                    (rsense-exit)))))

  ;; No lighters
  (hbin-remove-mm-lighter 'ruby-block-mode)
  (hbin-remove-mm-lighter 'ruby-tools-mode)

  (add-hook 'ruby-mode-hook
            (lambda ()
              (modify-syntax-entry ?$ "w")
              (modify-syntax-entry ?@ "w")
              (modify-syntax-entry ?: ".")))

  ;; Borrow from [doitian](http://blog.iany.me/2012/01/hilight-ruby-new-hash-in-emacs/)
  (font-lock-add-keywords
   'ruby-mode
   '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))))

  (define-key 'help-command "R" 'yari)
  (define-key ruby-mode-map (kbd "C-.") 'insert-arrow))

;;;###autoload
(defun insert-arrow ()
  "Insert arrow and put cursor at the right position."
  (interactive)
  (delete-horizontal-space t)
  (insert " => "))

;; hs-minor-mode for ruby mode
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

(provide 'prog-ruby)
