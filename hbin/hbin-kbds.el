;; hbin-kbds.el --- keyboard is my piano.
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

;; define my own keymap
(defvar hbin-map (make-sparse-keymap))
(global-set-key (kbd "M-s") hbin-map)

;;; Unbinding keys
(global-unset-key (kbd "C-SPC"))        ; conflict with IME
(global-unset-key (kbd "C-x C-p"))      ; used to mark page
(global-unset-key (kbd "C-x C-n"))      ; used to set-goal-column

;;; Translate C-h with C-? in any mode.
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(define-key key-translation-map [?\C-h] [?\C-?])

;;; Buffer manipulate
(global-set-key (kbd "C-c n") 'cleanup-region-or-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c C-c") 'whole-line-or-region-comment-dwim-2)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-k") 'kill-this-buffer)

;;; File manipulate
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)
(global-set-key (kbd "C-x d") 'delete-file-and-buffer)

;;; Edit
(global-set-key (kbd "M-'") 'match-paren)
(global-set-key (kbd "C-a") 'beginning-of-line++)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-^") 'join-line)
(global-set-key (kbd "M-@") 'hs-toggle-hiding)
(global-set-key (kbd "M-f") (lambda (arg) (interactive "^p") (forward-to-word arg)))

;; Vim lke scroll up/down line
(global-set-key (kbd "C-z") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-M-z") (lambda () (interactive) (scroll-down 1)))

;; Vim like open previous/next line
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-M-o") 'open-previous-line)

;; Move current line one line up/down
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

;;; Font size
(global-set-key (kbd "C-x C-+") 'text-scale-increase)
(global-set-key (kbd "C-x C--") 'text-scale-decrease)

;;; Easily navigate between recent buffers
(if (>= emacs-major-version 24)
    (progn
      (global-set-key (kbd "M-]") 'next-buffer)
      (global-set-key (kbd "M-[") 'previous-buffer))
  (progn
    (global-set-key (kbd "M-]") 'previous-buffer)
    (global-set-key (kbd "M-[") 'next-buffer)))

;;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)
(global-set-key (kbd "M-r") 'highlight-symbol-query-replace)

;;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-o") 'other-window)                               ; next window
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1))) ; back one

;; Window manipulate - use keypad
(global-set-key (kbd "<C-kp-6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-kp-4>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-kp-2>") 'enlarge-window)
(global-set-key (kbd "<C-kp-8>") 'shrink-window)
(global-set-key (kbd "<C-kp-7>") 'split-window-vertically)
(global-set-key (kbd "<C-kp-9>") 'split-window-horizontally)
(global-set-key (kbd "<C-kp-1>") 'delete-other-windows)
(global-set-key (kbd "<C-kp-3>") 'kill-this-buffer)
(global-set-key (kbd "<C-kp-0>") 'delete-window)
(global-set-key (kbd "<C-kp-5>") 'other-window)
(global-set-key (kbd "<C-kp-divide>") 'swap-windows)
(global-set-key (kbd "<C-kp-subtrcat>") 'previous-buffer)
(global-set-key (kbd "<C-kp-add>") 'next-buffer)
(global-set-key (kbd "<C-S-kp-begin>") (lambda () (interactive) (other-window -1)))

;;; Eshell and shell
(global-set-key (kbd "C-x m") 'eshell)                              ; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t))) ; Start a new eshell even if one is active.
(global-set-key (kbd "C-x C-m") 'shell)                             ; Start a regular shell if you prefer that.

;;; Flymake
(define-key hbin-map (kbd "f n") 'flymake-goto-next-error)
(define-key hbin-map (kbd "f p") 'flymake-goto-prev-error)

;;; Ack
(define-key hbin-map (kbd "a") 'ack)

;;; Imenu
(define-key hbin-map (kbd "i") 'imenu)

;;; Magit
(eval-after-load "magit" '(progn (global-set-key (kbd "C-c g") 'magit-status)))

;;; Smex
(eval-after-load "smex" '(progn (global-set-key (kbd "M-x") 'smex)))

;;; Org-mode
;; (define-key hbin-map (kbd "o") 'org-agenda)
(define-key hbin-map (kbd "o t") 'org-todo-list)
(define-key hbin-map (kbd "o l") 'org-store-link)
(define-key hbin-map (kbd "o c") 'org-capture)
(define-key hbin-map (kbd "o a") 'org-agenda)
(define-key hbin-map (kbd "o b") 'org-iswitchb)

;; This is a little hacky since VC doesn't support git add internally
(eval-after-load 'vc
  (define-key vc-prefix-map "i"
    '(lambda () (interactive)
       (if (not (eq 'Git (vc-backend buffer-file-name)))
           (vc-register)
         (shell-command (format "git add %s" buffer-file-name))
         (message "Staged changes.")))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-kbds)

;;;;;;; hbin-kbds.el END
