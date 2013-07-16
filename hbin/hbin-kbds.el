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
(global-unset-key (kbd "C-x C-p"))      ; used to mark page
(global-unset-key (kbd "C-x C-n"))      ; used to set-goal-column
(global-unset-key (kbd "C-x 0"))        ; used to delete-window
(global-unset-key (kbd "C-x 1"))        ; used to delete-other-windows
(global-unset-key (kbd "C-x 2"))        ; used to split-window-vertically
(global-unset-key (kbd "C-x 3"))        ; used to split-window-horizontally

;;; Help command
(global-set-key (kbd "<f1>") 'help-command)

;;; Translate C-h with C-? in any mode.
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(define-key key-translation-map [?\C-h] [?\C-?])

(global-set-key (kbd "C-\\") 'align-regexp)

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c n") 'cleanup-region-or-buffer)
(global-set-key (kbd "C-c x") 'execute-extended-command)

;; Start proced in a similar manner to dired
(global-set-key (kbd "C-x p") 'proced)
(global-set-key (kbd "C-x d") 'hbin-delete-file-and-buffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-i") 'imenu)

(global-set-key (kbd "C-a") 'beginning-of-line++)
(global-set-key (kbd "M-'") 'match-paren)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Vim lke scroll up/down line
(global-set-key (kbd "C-z") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-M-z") (lambda () (interactive) (scroll-down 1)))

;; Vim like open previous/next line
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-M-o") 'open-previous-line)

(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

;;; Font size
(global-set-key (kbd "C-x C-+") 'text-scale-increase)
(global-set-key (kbd "C-x C--") 'text-scale-decrease)

;;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)
(global-set-key (kbd "M-r") 'highlight-symbol-query-replace)

;;; Eshell and shell
(global-set-key (kbd "C-x m") 'eshell)                              ; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t))) ; Start a new eshell even if one is active.
(global-set-key (kbd "C-x C-m") 'shell)                             ; Start a regular shell if you prefer that.

;;; Easily navigate between recent buffers
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)

;;; Winner Mode
(winner-mode 1)
(global-set-key (kbd "C-x <left>") 'winner-undo)
(global-set-key (kbd "C-x <right>") 'winner-redo)

;;; Window switching
(windmove-default-keybindings 'super) ;; super+direction
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-4") 'delete-other-windows-vertically)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1))) ; back one
(global-set-key (kbd "M-k") 'kill-this-buffer)

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

;;; Ack
(define-key hbin-map (kbd "a") 'ack)

;;; Magit
(eval-after-load "magit" '(progn (global-set-key (kbd "C-c g") 'magit-status)))

;;; Smex
(eval-after-load "smex" '(progn (global-set-key (kbd "M-x") 'smex)))

;;; Textmate
(eval-after-load "textmate"
  '(progn (define-key *textmate-mode-map* (kbd "<C-tab>") nil)
          (global-set-key (kbd "M-[") 'textmate-shift-left)
          (global-set-key (kbd "M-]") 'textmate-shift-right)))

;;; Buffer move
(eval-after-load "buffer-move"
  '(progn
     (global-set-key (kbd "<S-s-up>")     'buf-move-up)
     (global-set-key (kbd "<S-s-down>")   'buf-move-down)
     (global-set-key (kbd "<S-s-left>")   'buf-move-left)
     (global-set-key (kbd "<S-s-right>")  'buf-move-right)))

;;; Browser kill ring
(eval-after-load "browse-kill-ring"
  '(progn (define-key hbin-map (kbd "k") 'browse-kill-ring)))

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
