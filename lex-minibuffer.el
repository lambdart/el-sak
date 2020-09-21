;;; lex-minibuffer.el --- Library Extension: Minibuffer -*- lexical-binding: t -*-
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
;; `Minibuffer' library extensions.
;;
;;; Code:

(require 'minibuffer)
(require 'help-fns)

(defmacro minibuffer-action (&rest body)
  "Check `minibuffer' if active and evaluate BODY forms (action)."
  `(when (minibufferp)
     (let ((candidate (car completion-all-sorted-completions)))
       ,@body)))

;;;###autoload
(defun minibuffer-kill-top-candidate ()
  "Put `minibuffer' completion candidate at the top of the `kill-ring'."
  (interactive)
  (minibuffer-action (kill-new `,candidate)))

;;;###autoload
(defun minibuffer-insert-top-candidate ()
  "Insert `minibuffer' completion candidate in current active buffer."
  (interactive)
  (minibuffer-action
   (with-minibuffer-selected-window
     (insert `,candidate))))

;;;###autoload
(defun minibuffer-describe-top-candidate ()
  "Describe symbol using top-most `minibuffer' completion candidate."
  (interactive)
  (when (minibufferp)
    (let* ((candidate (car completion-all-sorted-completions))
           (symbol (read candidate)))
      (when (symbolp symbol)
        (describe-symbol symbol)))))

(provide 'lex-minibuffer)
;;; lex-minibuffer.el ends here
