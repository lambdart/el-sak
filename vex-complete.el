;;; vex-complete.el --- summary -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
;; Version: 0.1
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

(require 'completion)

(defun complete-or-indent ()
  "Complete or indent."
  (interactive)
  (cond
    ((looking-at "\\_>") (complete nil))
    (t (indent-according-to-mode))))

(defun complete-at-point-or-indent ()
  "This smart tab is a `minibuffer' compliant.

It acts as usual in the `minibuffer'.
Case mark is active, indents region.
Case point is at the end of a symbol, expands it.
Or indents the current line."

  (interactive)
  (cond
    ((minibufferp)
      (unless (minibuffer-complete)
        (complete-symbol nil)))
    (mark-active (indent-region (region-beginning) (region-end)))
    ((looking-at "\\_>") (complete-symbol nil))
    (t (indent-according-to-mode))))

(provide 'vex-complete)
;;; vex-complete.el ends here
