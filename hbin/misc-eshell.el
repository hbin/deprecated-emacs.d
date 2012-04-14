;; misc-eshell.el --- Configuration for eshell and shell.
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
  (require 'tramp)                        ; 访问远程服务器
  (setq tramp-default-method "ssh")       ; 默认使用 SSH 协议

  (eval-after-load 'shell-mode
    '(progn
       ))

  (eval-after-load 'esh-opt
    '(progn
       ;; borrow from Starter-Kit
       (require 'em-term)
       (require 'em-cmpl)
       (require 'em-prompt)

       ;; (setenv "PAGER" "cat")
       (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")

       (setq eshell-cmpl-cycle-completions nil)
       (setq eshell-save-history-on-exit t)
       (setq eshell-buffer-shorthand t)
       (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

       ;; borrow from MasteringEmacs
       (require 'em-smart)
       (setq eshell-where-to-jump 'begin)
       (setq eshell-review-quick-commands nil)
       (setq eshell-smart-space-goes-to-end t)))

  (defface eshell-error-prompt-face
    '((((class color) (background dark)) (:foreground "red" :bold t))
      (((class color) (background light)) (:foreground "red" :bold t)))
    "Face for nonzero prompt results"
    :group 'eshell-prompt)

  (add-hook 'eshell-after-prompt-hook
            (defun eshell-exit-code-prompt-face ()
              (when (and eshell-last-command-status
                         (not (zerop eshell-last-command-status)))
                (let ((inhibit-read-only t))
                  (add-text-properties
                   (save-excursion (beginning-of-line) (point)) (point-max)
                   '(face eshell-error-prompt-face))))))

  ;; ANSI color must be here (before load eshell-mode)
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply))

;;;###autoload
(defun eshell/cls ()
  "Clear the eshell screen."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min)
                   (point-max))))

;;;###autoload
(defun eshell/cdg ()
  "Change directory to the (git) project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-eshell)

;;;; misc-eshell.el END
