;; misc-magit.el --- Configuration for magit
;;
;; Copyright (C) 2012 Huang Bin
;;
;; Author: Huang Bin <embrace.hbin@gmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary

;; Git for Emacs - awesome extension!

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

(require 'magit)

;; magit diff 时颜色区别
(eval-after-load 'magit
  (progn
    (set-face-foreground 'magit-diff-add "green4")
    (set-face-foreground 'magit-diff-del "red")))

;; diff-mode 也一样
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red")))

;;; Need flyspell against my poor english
(add-hook 'magit-log-edit-mode-hook 'turn-on-flyspell)

;; Full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; Back to previous window
(defun magit-quit-session ()
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;; Toggle ignore whitespace
(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (setq magit-diff-options (remove "-w" magit-diff-options))
    (add-to-list 'magit-diff-options "-w"))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-magit)

;;;;;; misc-magit.el END
