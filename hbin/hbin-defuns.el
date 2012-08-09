;; hbin-defuns.el --- Useful tricks, part of them stolen from others, thanks you all!
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

;; Jump between parent
(defun match-paren (arg)
  "Go to the matching parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

;; Smart beginning of line
(defun beginning-of-line++ ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

;; WholeLineOrRegion - http://www.emacswiki.org/emacs/WholeLineOrRegion
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (message "Copid line")
         (list (line-beginning-position) (line-beginning-position 2)))))
(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(defun delete-backward-space ()
  (interactive)
  (delete-horizontal-space t))

(defun delete-forward-space ()
  (interactive)
  (delete-region (point)
                 (progn (skip-chars-forward " \t")
                        (point))))

;; Behave like vi's o command
(defun open-next-line ()
  "Move to the next line and then opens a line."
  (interactive)
  (end-of-line)
  (open-line 1)
  (next-line 1)
  (indent-according-to-mode))

;; Behave like vi's O command
(defun open-previous-line ()
  "Open a new line before the current one."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun zap-up-to-char (arg char)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (interactive "p\ncZap up to char: ")
  (zap-to-char arg char)
  (insert char)
  (forward-char -1))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (if (region-active-p)
      (progn
        (indent-region (region-beginning) (region-end))
        (message "Indent selected region done."))
    (progn
      (indent-region (point-min) (point-max))
      (message "Indent buffer done."))))

(defun untabify-region-or-buffer ()
  "Untabify a region if selected, otherwise the whole buffer."
  (interactive)
  (if (region-active-p)
      (progn
        (untabify (region-beginning) (region-end))
        (message "Untabify selected region done."))
    (progn
      (untabify (point-min) (point-max))
      (message "Untabify buffer done."))))

(defun cleanup-region-or-buffer ()
  "Cleanup a region if selected, otherwise the whole buffer."
  (interactive)
  (if (region-active-p)
      (progn
        (delete-trailing-whitespace)
        (indent-region (region-beginning) (region-end))
        (untabify (region-beginning) (region-end))
        (message "Clean selected region done."))
    (progn
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max))
      (untabify (point-min) (point-max))
      (message "Clean buffer done."))))

(defun hbin-scroll-up-line ()
  "Compatible with Emacs 23, same as scroll-up-line in Emacs 24."
  (interactive)
  (scroll-up 1))

(defun hbin-scroll-down-line ()
  "Compatible with Emacs 23, same as scroll-down-lin in Emacs 24."
  (interactive)
  (scroll-down 1))

(defun comment-or-uncomment-region-or-line ()
  "comment or uncomment a region if selected, otherwise the current line."
  (interactive)
  (if (region-active-p)
      (progn
        (comment-or-uncomment-region (region-beginning) (region-end)))
    (progn
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

(defun copy-file-name-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(defun delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting"
  (interactive)
  (if (y-or-n-p "Delete file and buffer?")
      (progn (let ((filename (buffer-file-name)))
               (when filename
                 (delete-file filename)
                 (message "Deleted file %s" filename)))
             (kill-buffer))))

(defun kill-buffer-or-delete-window-dwim ()
  "Kill this buffer or delete window dwim."
  (interactive)
  (if (one-window-p)
      (kill-this-buffer)
    (delete-window)))

(defun swap-windows ()
  "Borrow from prelude."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;;;;; Scratch

;; If the *scratch* buffer is killed...
(defun kill-scratch-buffer ()
  (set-buffer (get-buffer-create "*scratch*"))
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (insert initial-scratch-message))

;; .....create it automatically
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

;;;;;;; Switch functions used for hooks

(defun turn-on-watchwords ()
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun turn-on-projectile-mode ()
  (interactive)
  (projectile-mode 1))

(defun turn-on-rainbow-delimiters-mode ()
  (interactive)
  (rainbow-delimiters-mode 1))

(defun turn-on-autopair-mode ()
  (interactive)
  (autopair-mode 1))

(defun turn-on-linum-mode ()
  (interactive)
  (linum-mode 1))

(defun turn-on-hs-mode ()
  (interactive)
  (hs-minor-mode 1))

;; (defun hbin-toggle-mode (&optional arg)
;;   "toggle mode example"
;;   (interactive)
;;   (setq mode
;;         (if (null arg) (not mode)
;;           (> (prefix-numeric-value arg) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hbin-defuns)

;;;;; hbin-defuns.el END
