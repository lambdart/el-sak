;;; lex-uu.el --- invoke unix utilities -*- lexical-binding: t -*-
;;
;; Author: lambdart <<lambdart@protonmail.com>>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: unix utilities processes process
;;
;; This file is NOT part of GNU Emacs.
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
;; This library add new functions to invoke unix utilities
;; from the Virtual Lisp Machine interpreter space (Emacs).
;;
;;; Code:

(require 'files)

(defgroup lex-uu nil
  "Unix utilities."
  :group 'extensions
  :group 'convenience)

(defcustom lex-opacity .9
  "Opacity default value.
The opacity value is a number/float from 0 to 1,
with zero being fully transparent and one (1 - 100%)
being fully opaque."
  :type 'float
  :group 'lex-uu
  :safe t)

(defcustom lex-transset "transset"
  "Program name that lets the user set the transparency
on a window."
  :type 'string
  :group 'lex-uu
  :safe t)

(defcustom lex-transset-args "-a"
  "Default arguments/switches for the `lex-transset' program."
  :type 'string
  :group 'lex-uu
  :safe t)

(defcustom lex-slock "slock"
  "X screen locker program."
  :type 'string
  :group 'lex-uu
  :safe t)

(defcustom lex-scrot "scrot"
  "Command line screen capture utility."
  :type 'string
  :group 'lex-uu
  :safe t)

(defcustom lex-scrot-dir
  (expand-file-name "scrot" user-emacs-directory)
  "The folder where captured screen images will be saved."
  :type 'string
  :group 'lex-uu
  :safe t)

(defcustom lex-mixer "mixer"
  "Mixer program – set/display soundcard mixer values."
  :type 'string
  :group 'lex-uu
  :safe t)

(defcustom lex-mixer-factor 5
  "Volume up/down factor."
  :type 'integer
  :group 'lex-uu
  :safe t)

(defun set-transparency (opacity &optional args)
  "Set OPACITY transparency passing ARGS to `lex-transset' program."
  ;; maps function arguments when called interactively
  (interactive
   (list
    ;; set opacity
    (read-number "Opacity: " lex-opacity)
    ;; verify universal argument (implicit)
    (when current-prefix-arg
      (read-string "Args: " lex-transset-args))))
  ;; verify if 'transset' executable is available
  (if (not (executable-find lex-transset))
      (message "Program %s not found" lex-transset)
    ;; invoke the command 'transset' asynchronous
    (async-shell-command
     (format "%s %s %.1f" lex-transset (or args lex-transset-args) opacity))))

;;;###autoload
(defun set-frame-transparency ()
  "Set transparency in current frame."
  (interactive)
  (call-interactively 'set-transparency))

;;;###autoload
(defun set-window-transparency (opacity)
  "Set OPACITY transparency in selected X window (including EMACS)."
  ;; map opacity argument when invoked interactively
  (interactive
   (list (read-number "Opacity: " lex-opacity)))
  ;; set transparency using 'transset' -c argument
  (set-transparency opacity "-c"))

;;;###autoload
(defun capture-screen (&optional dest)
  "Capture screen (an image) and save at DEST directory."
  (interactive
   (list
    ;; if prefix, read directory name
    (when current-prefix-arg
      (read-directory-name "Dir: " nil default-directory t))))
  ;; body:
  (let ((default-directory (expand-file-name
                            (or dest lex-scrot-dir)))
        (scrot (executable-find lex-scrot)))
    (cond
     ;; if screen capture utility
     (scrot
      (funcall 'start-process scrot nil scrot)
      (message "Image saved at %s" default-directory))
     ;; default
     (t
      (message "Command %s not found" lex-scrot)))))

;;;###autoload
(defun lock-screen ()
  "Lock screen using `lex-slock' utility."
  (interactive)
  (if (not (executable-find lex-slock))
      (message "Command %s not found" lex-slock)
    (async-shell-command lex-slock)))

;;;###autoload
(defun set-volume (value &optional direction)
  "Set volume VALUE in up or down DIRECTION."
  (interactive
   (list
    (read-number "Volume: " 50)))
  (let* ((mixer (executable-find lex-mixer))
         (direction (or direction :set))
         (cmd (cond
               ((eq direction :up)   (format "%s vol +%d" mixer value))
               ((eq direction :down) (format "%s vol -%d" mixer value))
               ((eq direction :set)  (format "%s vol %d" mixer value))
               ;; nop equivalent for this operation
               (t (format "%s vol -0" mixer)))))
    (cond
     ;; case mixer: raise volume
     (mixer
      (async-shell-command cmd))
     ;; default
     (t (message "Mixer command not found")))))

;;;###autoload
(defun increase-volume (&optional n)
  "Increase volume by a factor of 5.
If \\[universal-argument] is used, display a prompt
asking for the volume value - N."
  (interactive
   (list
    (when current-prefix-arg
      (read-number "Factor: " 5))))
  (let ((factor (or n lex-mixer-factor)))
    (set-volume factor :up)
    (message "Volume raised: +%d" factor)))

;;;###autoload
(defun lower-volume (&optional n)
  "Lower volume by a factor of 5.
If \\[universal-argument] is used, display a prompt
asking for the volume value - N."
  (interactive
   (list
    (when current-prefix-arg
      (read-number "Factor: " 5))))
  (let ((factor (or n lex-mixer-factor)))
    (set-volume factor :down)
    (message "Volume lower: -%d" factor)))

;;;###autoload
(defun mute-audio ()
  "Mute volume."
  (interactive)
  (set-volume 0 :set))

;;;###autoload
(defun export-pdf-to-text (pdf txt)
  "Convert a PDF to TXT file.

When \\[universal-argument] is used, asks for the
text file output name."

  ;; map function arguments
  (interactive
   (list
    (read-file-name "File:" nil nil t)
    (when current-prefix-arg
      (read-file-name "Fout:" nil nil))))
  ;; set auxiliary params
  (let* ((file (expand-file-name pdf))
         (fout (if (not txt) nil (expand-file-name txt)))
         (cmd (format "pdftotext '%s'" file)))
    ;; verify if file exists
    (when (file-exists-p file)
      ;; parse output file if necessary
      (if fout
          (setq cmd (concat cmd (format " '%s'" fout))))
      ;; finally execute pdftotext command
      (async-shell-command cmd))))

(provide 'lex-uu)

;;; lex-uu.el ends here
