;;; b-edit.el --- Basic and simple editing library
;;
;; -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Version: 0.1
;; URL: https://github.com/esac-io/b
;; Compatibility: GNU Emacs 25.x
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 esac
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'simple)

(defun transpose-region (arg)
  "Transpose region by one line, ARG set the direction.
If ARG positive UP otherwise DOWN."
  (unless (region-active-p)
    (error "Reggion is not active"))
  ;; step 1: identifying the text to cut.
  (let ((beg (region-beginning))
         (end (region-end))
         (dir (if (> arg 0) 1 -1))) ;; direction down or up
  ;; step 2: cut and paste
  (let ((text (delete-and-extract-region beg end)))
    (forward-line dir) ;; direction
    (insert text)
    ;; step 3: restore mark if necessary
    (when (region-active-p)
      (setq deactivate-mark nil)
      (set-mark (+ (point) (- beg end)))))))

(defun transpose-line (arg)
  "Transpose line ARG set the direction.
If ARG is positive UP else DOWN."
  ;; step 1: identifying the text to cut.
  (let ((orig (point))
         (beg (line-beginning-position))
         (end (1+ (line-end-position))))
    ;; step 2: cut and paste
    (let ((text (delete-and-extract-region beg end)))
      (forward-line arg)
      (insert text)
      ;; step 3: restore line position
      (forward-char (- orig end)))))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
      (indent-region (region-beginning) (region-end))
      (indent-buffer))))

(defun clone-line-or-region (&optional n)
  "Clone the current line or region N times.
If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers
will be duplicated."
  (interactive "p")
  (let ((beg (line-beginning-position))
         (end (line-end-position))
         (i (or n 1)))
    (when (region-active-p)
      (setq beg (region-beginning)
        end (region-end)))
    (let ((region (buffer-substring-no-properties beg end)))
      (while (> i 0)
        (goto-char end)
        (newline)
        (insert region)
        (setq i (1- i))))))

(defun transpose-lines-up ()
  "Transpose lines in down direction."
  (interactive)
  (if (region-active-p)
    (transpose-region 1)
    (transpose-line 1)))

(defun transpose-lines-down ()
  "Transpose lines in up direction."
  (interactive)
  (if (region-active-p)
    (transpose-region -1)
    (transpose-line -1)))

(defun transpose-word-left (n)
  "Transpose N words to the opposite direction (left)."
  (interactive "P")
  (transpose-words (- (or n 1))))

(defun copy-line-at-point (&optional arg)
  "Copy lines, do not kill then.
With prefix argument ARG, kill (copy) that many lines from point."
  (interactive "P")
  (let ((buffer-read-only t)
         (kill-read-only-ok t))
    (save-excursion
      (move-beginning-of-line nil)
      (kill-line arg))))

(provide 'b-simple)
;;; b-simple.el ends here
