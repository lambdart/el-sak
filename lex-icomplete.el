;;; lex-icomplete.el --- Library Extension: Icomplete -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
;; Version: 0.0.1 Alpha
;; URL: https://github.com/esac-io/lex
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
;; `Icomplete' library extensions.
;;
;;; Code:

(require 'icomplete)
(require 'minibuffer)

(defmacro icomplete-action-at-minibuffer (&rest body)
  "Check `minibuffer' if active and evaluate BODY forms (action)."
  `(when (minibufferp)
     (let ((candidate (car completion-all-sorted-completions)))
       ,@body)))

;;;###autoload
(defun icomplete-kill-candidate ()
  "Put `minibuffer' completion candidate at the top of the `kill-ring'."
  (interactive)
  (icomplete-action-at-minibuffer
   (kill-new `,candidate)))

;;;###autoload
(defun icomplete-insert-candidate ()
  "Insert `minibuffer' completion candidate in last active window."
  (interactive)
  (icomplete-action-at-minibuffer
   (with-minibuffer-selected-window
     (insert `,candidate))))

(provide 'lex-icomplete)
;;; lex-icomplete.el ends here
