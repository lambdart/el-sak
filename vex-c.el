;;; vex-c.el --- vanilla extensions
;;
;; -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Version: 0.1
;; URL: https://github.com/esac-io/vex
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
;; Basic Emacs C source functions.
;;
;;; Code:

(defun safe-funcall (func &rest args)
  "Call FUNC with ARGS, if it's bounded."
  (when (fboundp func)
    (funcall func args)))

(defun safe-kill-buffer (buffer-or-name)
  "Kill buffer specified by BUFFER-OR-NAME, if exists."
  (unless (or (stringp buffer-or-name)
            (bufferp buffer-or-name))
    (error "`buffer-or-name' must be a string or a buffer object"))
  (let ((buffer (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun safe-load-file (file)
  "Load FILE if exists."
  (if (file-exists-p file)
    (load (expand-file-name file) t nil nil)
    (message "File %s not found" file)))

(defun safe-add-subdirs-to-load-path (dir)
  "Add DIR and sub-directories to `load-path'."
  (let ((default-directory
          (expand-file-name dir user-emacs-directory)))
    (when (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

(defun safe-add-list-to-load-path (dir-list)
  "Add directories defined in DIR-LIST to `load-path'."
  (dolist (dir dir-list)
    (when (file-directory-p dir)
      (unless (member dir load-path)
        (append 'load-path
          (expand-file-name dir user-emacs-directory))))))

(defun select-minibuffer-window ()
  "Focus the active minibuffer, if available.

Bind this to `completion-list-mode-map' to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer."

  (interactive)
  (let ((window (active-minibuffer-window)))
    (when window
      (select-window window nil))))

(defun select-minibuffer-or-completions-window ()
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

(provide 'vex-c)
;;; vex-c.el ends here
