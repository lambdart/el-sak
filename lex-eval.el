;;; lex-eval.el --- eval extensions -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
;; Version: 0.0.1 Alpha
;; Keywords:
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

(require 'subr-x)

(defun command-history-candidates ()
  "Return \\[command-history] candidates."
  (let ((size (length command-history))
        (command nil)
        (candidates '()))
    ;; get candidates loop
    (dotimes (i size)
      (setq command (prin1-to-string (nth i command-history)))
      (when (not (or (string-empty-p command)
                     ;; ignore closure commands
                     (equal (string-match "^((" command) 0)))
        (push command candidates)))
    ;; return candidates
    candidates))

;;;###autoload
(defun eval-command-history ()
  "Eval previous command using `command-history-candidates'."
  (interactive)
  (let ((command
         (completing-read
          "Eval: " (command-history-candidates) nil 'confirm "(")))
    (save-restriction
      ;; save point
      (push-mark (point))
      ;; read and eval the command
      (eval (read command)))))

(provide 'lex-eval)

;;; lex-eval.el ends here
