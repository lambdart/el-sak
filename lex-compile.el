;;; lex-compile.el --- compile related functions/commands -*- lexical-binding: t -*-
;;
;; Maintainer: esac
;; Version: 0.0.3 Alpha
;; URL: https://github.com/esac-io/lex
;; Keywords: load-path load
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
;; This library provides interactive compile related functions and commands,
;; its use the compile-history to provide candidates to be selected using the
;; `minibuffer' completions system.
;;
;; Emacs Lisp has a “compiler” that translates functions written in Lisp
;; into a special representation called “byte-code” that can be executed
;; more efficiently. The compiler replaces Lisp function definitions with
;; byte-code. When a byte-code function is called, its definition is
;; evaluated by the “byte-code interpreter”.
;;
;;; Code:

(require 'compile)
(require 'subr-x)

;;;###autoload
(defun add-compile-command (command)
  "Add compile COMMAND to `compile-history' list."
  (interactive
   (list (read-string "Compile command: "
                      (format "make -C %s all" default-directory))))
  (unless (member command compile-history)
    (push command compile-history)
    (delete-dups compile-history)))

;;;###autoload
(defun compile-command-history (command)
  "Run compile COMMAND using `compile-history' as candidates source."
  (interactive
   ;; set compile command
   (list
    (completing-read "Compile command: "
                     compile-history
                     nil
                     'confirm ""
                     `(compile-history))))
  ;; compile using the compile command
  (when (not (string-empty-p compile-command))
    (compile command)))

;;;###autoload
(defun byte-compile-current-file ()
  "Save and byte compile current file."
  (interactive)
  ;; when the buffer is associate to a file
  (when buffer-file-name
    (cond
     ;; indicates if it's possible to byte-compile
     ((eq major-mode 'emacs-lisp-mode)
      ;; save the buffer before compile
      (save-buffer)
      (byte-compile-file buffer-file-name))
     ;; default
     (t (message "Was not possible to compile the file: %s" buffer-file-name)))))

;;;###autoload
(defun byte-compile-library (dir &optional load)
  "Byte compile a library, 'el's file inside a arbitrary DIR.

If LOAD \\[universal-argument] prefix is non-nil, load file
after compilation, be careful with this option.

Suggestion define a aliases for this: 'compile-library', inside your
init.el configuration."
  ;; maps: (dir load) arguments
  (interactive
   (list
    ;; dir
    (expand-file-name
     (read-directory-name
      "Dir: " (concat user-emacs-directory "site-lisp/") nil 'confirm))
    ;; load
    (if current-prefix-arg t nil)))
  ;; function body:
  (let ((files (directory-files dir t)))
    ;; let body:
    (dolist (file files)
      ;; byte-compile if file as the extension .el
      (when (equal (file-name-extension file) "el")
        (byte-compile-file file load)))))

;;;###autoload
(defalias 'compile-library 'byte-compile-library)

(provide 'lex-compile.el)

;;; lex-compile.el ends here
