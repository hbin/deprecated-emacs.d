;; hbin-misc.el --- Priceless configurations, make me comfortable.
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

;;;;;;; Here are for vendors
(require 'paredit)
(define-key paredit-mode-map (kbd "M-s") hbin-map)

(require 'autopair)
(setq autopair-blink nil)

(require 'whole-line-or-region)
(whole-line-or-region-mode)

(require 'flx-ido)
(flx-ido-mode 1)

;; Interactively Do hacks
(require 'ido-hacks)
(ido-hacks-mode 1)

(require 'smex)
(smex-initialize)

;; Textmate like find file
(require 'textmate)
(setq textmate-use-file-cache nil)

;; Colorful parenthesis
(require 'rainbow-delimiters)

;; Visible tree-like undo history
(require 'undo-tree)
(global-undo-tree-mode)

;; Key chords
(require 'key-chord)
(key-chord-mode +1)

;; Popup window manager
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; Turn off linum mode within some temp buffers
(require 'linum-off)

;; Show FIXME/TODO/BUG(...)
(require 'fic-mode)
(setq fic-highlighted-words
      '("FIXME" "TODO" "BUG" "OPTIMIZE" "HACK" "REVIEW"))

;; Look through everything killed
(require 'browse-kill-ring)

;; On-the-fly syntax checks
(require 'flycheck)

;; Ignore '/'
(require 'ffap)
(defvar ffap-c-commment-regexp "^/\\*+"
  "Matches an opening C-style comment, like \"/***\".")

(defadvice ffap-file-at-point (after avoid-c-comments activate)
  "Don't return paths like \"/******\" unless they actually exist.
This fixes the bug where ido would try to suggest a C-style
comment as a filename."
  (ignore-errors
    (when (and ad-return-value
               (string-match-p ffap-c-commment-regexp
                               ad-return-value)
               (not (ffap-file-exists-string ad-return-value)))
      (setq ad-return-value nil))))

(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  "ffap ignore the '/'"
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)

;;;;;;; Here are for trickers
(require 'misc)
(require 'misc-ac)                       ; Auto Complete
(require 'misc-yas)                      ; Yasnippet
(require 'misc-doc)                      ; Documents
(require 'misc-tags)                     ; Navigation in project
(require 'misc-hist)                     ; Save some history
(require 'misc-mark)                     ; Highlight and mark symbols
(require 'misc-dired)                    ; Extends build-in dired
(require 'misc-magit)                    ; Awesome extension!!!!
(require 'misc-eshell)                   ; Eshell
(require 'misc-ispell)                   ; Spell check
(require 'misc-ibuffer)                  ; Nice Ibuffer
(require 'misc-diminish)                 ; Clean up the mode line
(require 'misc-navigation)               ; Fast navigation

;;; load Mac OS X config if needed, unfortunately, I don't need it ;-)
(when (string= system-type "Darwin")
  (require 'misc-mac))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-misc)

;;;;;; hbin-misc.el END
