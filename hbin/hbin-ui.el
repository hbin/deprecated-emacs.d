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
  (tool-bar-mode -1)           ; 不需要工具栏
  (scroll-bar-mode -1)         ; 不需要滚动条
  (set-fringe-mode '(4 . 1))   ; 小的 fringe 边距
  (blink-cursor-mode -1)       ; 光标不闪
  (tooltip-mode -1)
  (mouse-wheel-mode t))

;; Fonts - Monaco for Emacs 24 and DejaVu for previous versions.
(if (>= emacs-major-version 24)
    (progn
      (set-default-font "Monaco-12")         ; Monaco is the best font for programming
      (set-fontset-font (frame-parameter nil 'font) 'han '("WenQuanYi Micro Hei Mono" . "unicode-bmp")))
  (progn
    (set-default-font "DejaVu Sans Mono-12") ; Dejavu Mono is very beautiful too
    (set-fontset-font (frame-parameter nil 'font) 'han '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))))

;; Themes
(if (>= emacs-major-version 24)
    (progn
      (add-subfolders-to-theme-load-path themes-dir)
      (load-theme 'solarized-dark t))
  (progn
    (require 'color-theme)
    (require 'color-theme-solarized)
    (color-theme-initialize)
    (color-theme-solarized-dark)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-ui)

;;;;;; hbin-ui.el END
