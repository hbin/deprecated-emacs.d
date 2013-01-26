;; hbin-ui.el --- Eye candy Emacs
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
(setq inhibit-startup-screen t)

;; Set frame title
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

(defun hbin-frame-init (frame)
  "Custom behaviours for new frames."
  (tooltip-mode -1)            ; 不要 tooltip
  (menu-bar-mode -1)           ; 不要菜单栏
  (tool-bar-mode -1)           ; 不需要工具栏
  (scroll-bar-mode -1)         ; 不需要滚动条
  (set-fringe-mode '(1 . 1))   ; 小的 fringe 边距
  (blink-cursor-mode -1)       ; 光标不闪
  (mouse-wheel-mode t))

;; run now
(hbin-frame-init (selected-frame))

;; and later
(add-hook 'after-make-frame-functions 'hbin-frame-init)

;; Fonts and Themes
;; Monaco: http://s.yunio.com/3FuQfa
;; Menlo: http://s.yunio.com/8XBaSx
;; YaHei Consolas Hybrid: http://s.yunio.com/ZFORNb
(defcustom hbin-frame-font "Menlo:pixelsize=17" "Default font")
(defcustom hbin-frame-font-chinese "YaHei Consolas Hybrid:pixelsize=17" "Chinese font")

(set-frame-font hbin-frame-font)
(set-fontset-font "fontset-default" 'chinese-gbk hbin-frame-font-chinese)
(add-to-list 'default-frame-alist (cons 'font hbin-frame-font))

(add-subfolders-to-theme-load-path themes-dir)
(load-theme 'solarized-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-ui)

;;;;;; hbin-ui.el END
