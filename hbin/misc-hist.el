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

;; Auto Complete cache
(setq ac-comphist-file (concat tmp-dir "ac.cache"))

;; Ido cache
(setq ido-save-directory-list-file (concat tmp-dir "ido.cache"))

;; 保存书签历史
(setq bookmark-default-file (concat tmp-dir "bookmarks.cache"))

;; 保存游标在访问过文件中的位置
(setq save-place-file (concat tmp-dir "saveplace.cache"))
(setq-default save-place t)
(require 'saveplace)

;; savehist: save some history
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat tmp-dir "savehist.cache"))
(savehist-mode t)

;; 将备份文件和自动保存临时文件放到临时文件夹
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-hist)

;;;; misc-hist.el END
