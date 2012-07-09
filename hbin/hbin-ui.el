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

;; turn off mouse interface early in startup to avoid momentary dispaly
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (menu-bar-mode -1)           ; 不要菜单栏
  (tool-bar-mode -1)           ; 不需要工具栏
  (scroll-bar-mode -1)         ; 不需要滚动条
  (set-fringe-mode '(0 . 1))   ; 小的 fringe 边距
  (blink-cursor-mode -1)       ; 光标不闪
  (tooltip-mode -1)
  (mouse-wheel-mode t))

;; Fonts and Themes
(if (>= emacs-major-version 24)
    (progn
      (set-default-font "Monaco-14")        ; To get Monaco -> http://d.yun.io/qQhWRj
      (set-fontset-font (frame-parameter nil 'font) 'han '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
      (add-subfolders-to-theme-load-path themes-dir)
      (load-theme 'solarized-dark t))
  (progn
    (set-default-font "Menlo-14")           ; Menlo is very beautiful too
    (set-fontset-font (frame-parameter nil 'font) 'han '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
    (require 'color-theme)
    (require 'color-theme-molokai)
    (color-theme-initialize)
    (color-theme-molokai)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-ui)

;;;;;; hbin-ui.el END
