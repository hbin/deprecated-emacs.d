;; misc-ac.el --- Configuration for Auto Completion
;;
;; Copyright (C) 2012 Huang Bin
;;
;; Author: Huang Bin <embrace.hbin@gmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary

;; Auto Completion - http://cx4a.org/software/auto-complete/manual.html#Introduction

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

(require 'auto-complete-config)

;; ac-dictionaries for major modes
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))

;; ac common settings
(setq ac-use-quick-help nil)                            ; 不需要 quick-help
(setq ac-auto-show-menu nil)                            ; 取消默认弹出补全菜单
(setq ac-menu-height 15)                                ; ....补全菜单长一点
(setq ac-auto-start nil)                                ; 不要自动补全
(setq ac-trigger-key "TAB")                             ; 按下 TAB 后开始补全
(setq ac-use-menu-map t)                                ; 当补全列表展开时...
(define-key ac-menu-map (kbd "C-n") 'ac-next)           ; ...可以使用 C-n....
(define-key ac-menu-map (kbd "C-p") 'ac-previous)       ; 和 C-p 上下移动待选项

;; Key bindings
(define-key ac-completing-map (kbd "TAB") 'ac-complete) ; 使用 Tab 键补全
(define-key ac-mode-map (kbd "M-/") 'auto-complete)     ; 按 M-/ 弹出补全下拉菜单
;; (define-key ac-completing-map (kbd "RET") nil)       ; 当补全列表弹出时候，回车不补全

;; Workarounds
(setq ac-stop-flymake-on-completing t)
(ac-flyspell-workaround)

;; Override this function to fix compatibility issue with yasnippet-0.7.0.
(defun ac-yasnippet-candidates ()
  (with-no-warnings
    (if (fboundp 'yas/get-snippet-tables)
        (if (null (help-function-arglist 'yas/get-snippet-tables))
            ;; >0.7.0
            (apply 'append (mapcar 'ac-yasnippet-candidate-1 (yas/get-snippet-tables)))
          ;; 0.6.0 < x < 0.7.0
          (apply 'append (mapcar 'ac-yasnippet-candidate-1 (yas/get-snippet-tables major-mode))))
      (let ((table
             (if (fboundp 'yas/snippet-table)
                 ;; <0.6.0
                 (yas/snippet-table major-mode)
               ;; 0.6.0
               (yas/current-snippet-table))))
        (if table
            (ac-yasnippet-candidate-1 table))))))

;; Override the default settings
(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-filename))

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features
                             ac-source-functions
                             ac-source-variables
                             ac-source-symbols) ac-sources)))

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-gtags) ac-sources)))

(defun ac-ruby-mode-setup ()
  (setq ac-sources (append '(ac-source-gtags) ac-sources)))

(defun ac-css-mode-setup ()
  (setq ac-sources (append '(ac-source-css-property) ac-sources)))

(defun ac-config-default ()
  (setq-default ac-sources '(ac-source-yasnippet ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-ac)

;;;;; misc-ac.el END
