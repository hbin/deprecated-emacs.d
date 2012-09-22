;; misc-hist.el --- Configuration for cache and history.
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

;; 将备份文件和自动保存临时文件放到临时文件夹
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Save some history/cache
(setq ac-comphist-file (concat tmp-dir "ac.cache"))                 ; Auto Complete history
(setq ido-save-directory-list-file (concat tmp-dir "ido.cache"))    ; Ido history
(setq bookmark-default-file (concat tmp-dir "bookmarks.cache"))     ; Bookmark history
(setq smex-save-file (concat tmp-dir "smex.cache"))                 ; Smex history

;; Auto-save
(setq auto-save-directory (concat tmp-dir "autosave")
      auto-save-hash-directory (concat tmp-dir "autosave-hash")
      auto-save-directory-fallback "/tmp/"
      auto-save-list-file-prefix (concat tmp-dir "autosave/autosave-")
      auto-save-hash-p nil
      auto-save-timeout 100
      auto-save-interval 300)
(unless (file-exists-p auto-save-directory)
  (make-directory auto-save-directory))

;; Save place
(setq save-place-file (concat tmp-dir "saveplace.cache"))
(setq-default save-place t)
(require 'saveplace)

;; savehist
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat tmp-dir "savehist.cache"))
(savehist-mode t)

;; save recent files
(setq recentf-save-file (concat tmp-dir "recentf.cache")
      recentf-max-saved-items 200
      recentf-max-menu-items 15)

;; eshell
(require 'eshell)
(setq eshell-directory-name (concat tmp-dir "eshell/"))

;; Desktop
(require 'desktop)

;; save the desktop file automatically if it already exists
(desktop-save-mode 1)

;; use only one desktop
(setq desktop-path (list tmp-dir))
(setq desktop-dirname tmp-dir)
(setq desktop-base-file-name "desktop.cache")

(setq desktop-restore-eager 15)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; specify buffers which should not be saved, by name or by mode
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
          '(lambda ()
             (setq desktop-dirname-tmp desktop-dirname)
             (desktop-remove)
             (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
          '(lambda ()
             (if (saved-session)
                 (if (not (y-or-n-p "Restore desktop? "))
                     (session-restore)))))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-hist)

;;;; misc-hist.el END
