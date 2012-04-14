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

(autoload 'yari-helm "yari" "View RDoc" t)

;;;###autoload
(progn
  (require 'rvm)

  (rvm-use-default)

  (eval-after-load 'ruby-mode
    '(progn
       (require 'ruby-end)              ;  Lazy.......
       (require 'ruby-block)            ;  ....Loading

       ;; Ruby-End
       (setq ruby-end-insert-newline nil)

       ;; Ruby-Block
       (ruby-block-mode t)                      ; highlight matching block of ruby
       (setq ruby-block-highlight-toggle t)     ; display block to minibuffer and do overlay

       ;; RSense - http://cx4a.org/software/rsense/manual.html#What_is_RSense_
       (setq rsense-home (expand-file-name (concat utils-dir "rsense")))
       (add-to-list 'load-path (concat (concat utils-dir "rsense") "/etc"))
       (require 'rsense)

       ;; Ruby key Binding
       (define-key 'help-command "r" 'yari-helm)
       (define-key ruby-mode-map (kbd "C-.") 'insert-arrow)
       (define-key ruby-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region-or-line))))

;;;###autoload
(defun insert-arrow ()
  "Insert arrow and put cursor at the right position."
  (interactive)
  (delete-horizontal-space)
  (let ((char (char-after (point))))
    (if (and (characterp char)
             (char-equal char ?%))
        (progn (insert " =>  ")
               (backward-char))
      (insert " => "))))

(provide 'prog-ruby)
