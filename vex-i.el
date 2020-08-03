;;; vex-i.el --- Interactive extensions -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
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
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
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
;;; Code:

(require 'compile)
(require 'recentf)
(require 'icomplete)

(eval-when-compile
  (require 'cl-macs))

(defun recentf-candidates ()
  "Get recent files candidates."
  (mapcar 'abbreviate-file-name recentf-list))

;;;###autoload
(defun recentf-find-file ()
  "Find recent file."
  (interactive)
  (let ((candidate
         (completing-read "Recentf: "
                          (recentf-candidates) nil t)))
    ;; find files
    (find-file candidate)))

(defun kill-ring-candidates ()
  "Return `kill-ring' candidates."
  (let ((candidates
         (cl-loop with candidates = (delete-dups kill-ring)
                  for c in candidates
                  unless (or (< (length c) 4)
                             (string-match "\\`[\n[:blank:]]+\\'" c))
                  collect c)))
    candidates))

;;;###autoload
(defun insert-kill-ring ()
  "Insert text from `kill-ring' candidates."
  (interactive)
  (let ((candidates (kill-ring-candidates)))
    (if (not candidates)
        (message "Kill ring is empty"))
    (insert
     (completing-read "Kill-ring: " candidates nil t))))

;;;###autoload
(defun compile-history ()
  "Compile using `compile-history' as candidates."
  (interactive)
  (let ((candidates compile-history))
    (compile
     (completing-read "Command: "
                      candidates nil 'confirm "" `(compile-history)))))

(defun parse-mark-line-to-string (pos)
  "Return line string at position POS."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((line (car (split-string (thing-at-point 'line) "[\n\r]"))))
      (remove-text-properties 0 (length line) '(read-only) line)
      (if (string= "" line)
          "<EMPTY LINE>"
        line))))

(defun mark-ring-candidates ()
  "Return parsed mark ring candidates."
  (cl-loop with marks = (if (mark t)
                            (cons (mark-marker) mark-ring)
                          mark-ring)
           for mark in marks
           with max-line-number = (line-number-at-pos (point-max))
           with width = (length (number-to-string max-line-number))
           for m = (format (concat "%" (number-to-string width) "d: %s")
                           (line-number-at-pos mark)
                           (parse-mark-line-to-string mark))
           unless (and recip (assoc m recip))
           collect (cons m mark) into recip
           finally return recip))

;;;###autoload
(defun mark-goto-char ()
  "Browse `mark-ring' interactively and jump to the selected position."
  (interactive)
  (let ((candidates (mark-ring-candidates))
        (position nil))
    (cond
     ;; if not candidates nothing to do, logs
     ;; and leave
     ((not candidates)
      (message "Mark ring is empty"))
     ;; else goto position
     (t
      (setq position
            (completing-read "Goto: " candidates nil t))
      (goto-char (cdr (assoc position candidates)))))))

(provide 'vex-i)
;;; vex-i.el ends here
