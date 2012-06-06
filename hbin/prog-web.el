;; prog-web.el --- Enhance HTML & CSS
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

;; eye candy rainbow mode
(autoload 'rainbow-mode "rainbow-mode" "Display color names with colored background" t)

;; zencoding
(autoload 'zencoding-mode "zencoding-mode" "a neat way to write html-like documents" t)

;;;###autoload
(progn
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'sgml-mode-hook 'rainbow-mode)
  (add-hook 'sgml-mode-hook 'zencoding-mode)

  (custom-set-variables '(css-indent-offset 2)))

(provide 'prog-web)
