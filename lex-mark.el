;;; lex-mark.el --- mark related functions -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Version: 0.0.2 Alpha
;; URL: https://github.com/esac-io/lex
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
;; This library extends other libraries (adds more (clever) functions to it),
;; this functions should be small in code size and its goals are to engage
;; the overall usability, some of them has the prefix 'safe' which indicates
;; their purposed to be a more safer version compered to the
;; original counterparts.
;;
;; Others don't have any prefix and this is intentional,
;; as they are basic functions and will be called from the
;; interactive prompt (minibuffer) or keybinded latter.
;;
;; The functions names follows the code convection:
;; ([prefix-]action-target-object).
;;
;;; Code:

(require 'simple)

(eval-when-compile
  (require 'cl-macs))

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
(defun goto-mark ()
  "Goto selected `mark' position."
  (interactive)
  (let ((candidates (mark-ring-candidates))
        (candidate nil))
    (cond
     ;; no candidates, logs and leave
     ((not candidates)
      (message "Mark ring is empty"))
     ;; default, goto position (char in the buffer)
     (t
      ;; select candidate from mark-ring-candidates
      (setq candidate (completing-read "Goto: " candidates nil t))
      ;; mark to char and finally go to it!
      (goto-char (cdr (assoc candidate candidates)))))))

(provide 'lex-mark)

;;; lex-mark.el ends here
