;; hbin-editor.el --- Enhance the editor
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

;;;;;;;; Here are for defaults

;; mode line 设定
(line-number-mode t)                     ; 显示行号
(column-number-mode t)                   ; 显示列号
(size-indication-mode t)                 ; 显示文件大小

(global-auto-revert-mode t)              ; 当 Buffer 访问的文件修改时，自动更新 Buffer 中的内容
(delete-selection-mode t)                ; 有选择域时，先删除再插入

(require 'linum)
(setq linum-format "%4d ")               ; 自定义行号格式
(global-linum-mode -1)                   ; 不显示 Linum 行号

(show-paren-mode t)                      ; 显示匹配括号
(setq show-paren-delay 0)                ; 匹配无延时
(setq show-paren-style 'parenthesis)     ; 只高亮显示匹配的括号

(toggle-indicate-empty-lines 1)

(mouse-avoidance-mode 'exile)            ; 鼠标自动移动到右上角，以免挡住视线
(fset 'yes-or-no-p 'y-or-n-p)            ; 以 y/n 代表 yes/no

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)      ; 空格代替制表符
(setq-default imenu-auto-rescan t)       ; Rescanning Imenu automatically
(setq-default truncate-lines t)          ; Disable line wrap

(setq suggest-key-bindings 1)            ; 当使用命令后，过 1 秒显示绑定的键
(setq require-final-newline t)           ; 存盘的时候，要求最后一个是换行符

(setq delete-by-moving-to-trash t)       ; 删除的文件放到回收站
(setq dired-recursive-deletes t)         ; 允许删除非空文件夹
(setq dired-recursive-copies t)          ; 允许复制非空文件夹

;; 平滑滚动
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; 鼠标滚动
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control) . nil)))

;; Interactively Do
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-max-directory-size 100000
      ido-default-file-method 'selected-window)

;; Ido 匹配时,忽略某些文件
(setq ido-ignore-extensions t)

;; Minibuffer 补全
(icomplete-mode +1)

;; Sweet window-splits
(defadvice split-window-right (after balance activate) (balance-windows))
(defadvice delete-window (after balance activate) (balance-windows))

;; 存盘前删除行末多余的空格/空行
(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

;; 存盘前添加 Time-stamp
(setq time-stamp-active t                ; do enable time-stamps
      time-stamp-line-limit 10           ; check first 10 buffer lines for Time-stamp: <>
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; Buffer 重名时，自动更改名字以区别
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; 系统文件编码统一为 UTF-8，文件格式为 unix (LF)
(require 'un-define "un-define" t)
(set-buffer-file-coding-system 'utf-8 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)

;; Shared clipboard with other X clients
(when (< emacs-major-version 24)
  (setq x-select-enable-clipboard t
        interprogram-paste-function
        'x-cut-buffer-or-selection-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-editor)

;;;;;;; hbin-editor.el END
