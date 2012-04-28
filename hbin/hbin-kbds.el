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

;; Unbinding keys
(global-unset-key (kbd "C-SPC")) ; conflict with IME

;;; Translate C-h with C-? in any mode.
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key key-translation-map [?\C-h] [?\C-?])

;;; Edit
(global-set-key (kbd "C-'") 'match-paren)
(global-set-key (kbd "C-a") 'beginning-of-line++)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-^") 'join-line)

;; Vim like open previous/next line
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-M-o") 'open-previous-line)

;; Vim like scroll up/down by line/page
(global-set-key (kbd "C-v") 'hbin-scroll-up-line)
(global-set-key (kbd "M-v") 'hbin-scroll-down-line)
(global-set-key (kbd "M-V") 'scroll-down)
(global-set-key (kbd "C-S-V") 'scroll-up)

;; Move current line one line up/down
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

;; Delete(kill) forward/backward as what I mean.
(global-set-key (kbd "M-d") 'kill-space-or-word-forward-dwim)
(global-set-key (kbd "C-M-h") 'kill-space-or-word-backward-dwim)

;; Cleanup(untabify and indent) active region if selected, otherwise the whole buffer
(global-set-key (kbd "C-c n") 'cleanup-region-or-buffer)

;; Comment-or-uncomment active region if selected, otherwise current line
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)

;;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;; Buffer
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Easily navigate between recent buffers
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

;;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini) ; better than helm-buffers-list
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
(global-set-key (kbd "C-x C-o") 'helm-occur)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-c C-f") 'helm-find-files)

;;; Amazing expand-region
(global-set-key (kbd "M-h") 'er/expand-region)

;;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-0") 'delete-window)
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
(global-set-key (kbd "<C-kp-divide>") 'previous-buffer)
(global-set-key (kbd "<C-kp-multiply>") 'next-buffer)
(global-set-key (kbd "<C-S-kp-begin>") (lambda () (interactive) (other-window -1)))

;;; Eshell and shell
(global-set-key (kbd "C-x m") 'eshell)                              ; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t))) ; Start a new eshell even if one is active.
(global-set-key (kbd "C-x C-m") 'shell)                             ; Start a regular shell if you prefer that.

;;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

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
