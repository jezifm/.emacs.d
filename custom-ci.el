;;; custom-ci.el --- Custom version of change-inner  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jezrael Arciaga

;; Author: Jezrael Arciaga <jezarciaga@gmail.com>
;; Keywords: change, inner

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun jez/change-inner* (yank? search-forward-char)
  "Custom version of change-inner* to work with `jez/copy-inner'

Works like vim's ci command. Takes a char, like ( or \" and
kills the innards of the first ancestor semantic unit starting with that char."
  (let* ((expand-region-fast-keys-enabled nil)
         (char (or search-forward-char
                   (char-to-string
                    (read-char
                     (if yank?
                         "Yank inner, starting with:"
                       "Change inner, starting with:")))))
         (q-char (regexp-quote char))
         (starting-point (point)))
    (flet ((message (&rest args) nil))
      (er--expand-region-1)
      (er--expand-region-1)
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er--expand-region-1))
      (if (not (looking-at q-char))
          (if search-forward-char
              (error "Couldn't find any expansion starting with %S" char)
            (goto-char starting-point)
            (setq mark-active nil)
            (change-inner* yank? char))
        (er/contract-region 1)
        (if yank?
            (progn
              (copy-region-as-kill (region-beginning) (region-end))
              (ci--flash-region (region-beginning) (region-end))
              (goto-char starting-point))
          (kill-region (region-beginning) (region-end)))))))

(defun jez/change-outer* (yank? search-forward-char)
  "Custom version of change-inner* to work with `jez/copy-inner'

Works like vim's ci command. Takes a char, like ( or \" and
kills the first ancestor semantic unit starting with that char."
  (let* ((expand-region-fast-keys-enabled nil)
         (char (or search-forward-char
                   (char-to-string
                    (read-char
                     (if yank?
                         "Yank outer, starting with:"
                       "Change outer, starting with:")))))
         (q-char (regexp-quote char))
         (starting-point (point)))
    (flet ((message (&rest args) nil))
      (when (looking-at q-char)
        (er/expand-region 1))
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er/expand-region 1))
      (if (not (looking-at q-char))
          (if search-forward-char
              (error "Couldn't find any expansion starting with %S" char)
            (goto-char starting-point)
            (setq mark-active nil)
            (change-outer* yank? char))
        (if yank?
            (progn
              (copy-region-as-kill (region-beginning) (region-end))
              (ci--flash-region (region-beginning) (region-end))
              (goto-char starting-point))
          (kill-region (region-beginning) (region-end)))))))

(defun jez/copy-inner ()
  "Customize version of copy inner to work with single quote(')"
  (interactive)
  (let* ((symbol (char-to-string (read-char "Yank inner, starting with:"))))
    (if (equal symbol "'")
	(save-excursion
	  (let* ((start (1+ (search-backward symbol)))
		 (end (1- (search-forward symbol nil nil 2))))
	    (kill-ring-save start end)
	    (message "Copied: %s" (buffer-substring start end))))
      (jez/change-inner* t symbol))))

(defun jez/copy-outer ()
  "Customize version of copy inner to work with single quote(')"
  (interactive)
  (let* ((symbol (char-to-string (read-char "Yank outer, starting with:"))))
    (if (equal symbol "'")
	(save-excursion
	  (let* ((start (search-backward symbol))
		 (end (search-forward symbol nil nil 2)))
	    (kill-ring-save start end)
	    (message "Copied: %s" (buffer-substring start end))))
      (jez/change-inner* t symbol))))

(provide 'custom-ci)
;;; custom-ci.el ends here
