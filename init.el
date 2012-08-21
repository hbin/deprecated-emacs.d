;; init.el --- Emacs for hbin starts from here, let's go!
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
(require 'cl)

;; Who am I?
(setq user-full-name "Huang Bin")
(setq user-mail-address "embrace.hbin@gmail.com")
(setq global-mode-string (message "  %s  " user-full-name))

;; debug begin, show error stack trace.
;; (setq debug-on-error t)

(defvar vendor-dir (concat user-emacs-directory "vendor/")
  "The root folder of external packages")

(defvar themes-dir (concat user-emacs-directory "themes/")
  "The root folder of themes")

(defvar tmp-dir (concat user-emacs-directory "temp/")
  "The root folder of temps")

;; Borrow from bbatsov's prelude to create automatically.
(unless (file-exists-p tmp-dir)
  (make-directory tmp-dir))

(defun add-subfolders-to-load-path (parent-dir)
  "Add subfolders to load path"
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(defun add-subfolders-to-theme-load-path (parent-dir)
  "Add subfolders to theme load path"
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'custom-theme-load-path name)))))

;; add the first level subfolders automatically
(add-subfolders-to-load-path user-emacs-directory)
(add-subfolders-to-load-path vendor-dir)
(add-subfolders-to-load-path themes-dir)

;; load configurations.
(require 'hbin-ui)                      ; eye candy
(require 'hbin-editor)                  ; enhance the editor
(require 'hbin-defuns)                  ; useful tricks
(require 'hbin-kbds)                    ; key bindings
(require 'hbin-misc)                    ; miscellaneous
(require 'hbin-prog)                    ; programming

;; set an explicit file to customization created via the UI
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;;;;;;;;;;; init.el END
