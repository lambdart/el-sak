;;; lex-minibuffer.el --- minibuffer related extentions -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: minibuffer interactive command
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
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
;; This library provides more commands to handle generic actions to
;; the `minibuffer' (with is a terrible name for a command prompt),
;; Some commands are related to `minibuffer' window selection, others
;; with copy/kill/insert/describe the top candidate.
;;
;; A “minibuffer” is a special buffer that Emacs commands use to read
;; arguments more complicated than the single numeric prefix argument.
;; These arguments include file names, buffer names, and command names (as
;; in ‘M-x’). The minibuffer is displayed on the bottom line of the frame,
;; in the same place as the echo area (*note The Echo Area::), but only
;; while it is in use for reading an argument.
;;
;;; Code:

(require 'delsel)
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
     (insert `,candidate)
     (minibuffer-keyboard-quit))))

;;;###autoload
(defun minibuffer-yank-top-candidate ()
  "Insert `minibuffer' completion candidate inline."
  (interactive)
  (minibuffer-action
   (beginning-of-line)
   (kill-new `,candidate)
   (yank)
   (kill-line)))

;;;###autoload
(defun minibuffer-describe-top-candidate ()
  "Describe symbol using top-most `minibuffer' completion candidate."
  (interactive)
  (minibuffer-action
   (let ((symbol (read `,candidate)))
     (when (symbolp symbol)
       (describe-symbol symbol)
       (select-window (minibuffer-window))))))

;;;###autoload
(defun goto-minibuffer-window ()
  "Go to the active minibuffer, if available.
Bind this to `completion-list-mode-map' to easily jump
between the list of collection candidates present in
the \\*Completions\\* buffer and the minibuffer."
  (interactive)
  (let ((window (active-minibuffer-window)))
    (when window
      (select-window window nil))))

;;;###autoload
(defun goto-minibuffer-or-call-it ()
  "Go to minibuffer window or call `execute-extended-command'."
  (interactive)
  (let ((window (active-minibuffer-window)))
    (if (not window)
        (call-interactively 'execute-extended-command)
      (select-window window nil))))

;;;###autoload
(defun goto-completions-window ()
  "Go to the active completions window, if available."
  (interactive)
  (let ((window (get-buffer-window "*Completions*")))
    (when window
      (select-window window nil))))

;;;###autoload
(defun goto-minibuffer-or-completions-window ()
  "Focus the active minibuffer or the \\*Completions\\*.
If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two."
  (interactive)
  (let ((minibuffer-window (active-minibuffer-window))
        (completions-window (get-buffer-window "*Completions*")))
    (cond
     ((and minibuffer-window (not (minibufferp)))
      (select-window minibuffer-window nil))
     ((and completions-window (get-buffer "*Completions*"))
      (select-window completions-window t)))))

(provide 'lex-minibuffer)

;;; lex-minibuffer.el ends here
