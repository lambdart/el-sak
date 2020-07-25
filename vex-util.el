;;; vex.el --- vanilla Emacs extensions -*- lexical-binding: t -*-
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
;; This package adds utility functions to Emacs/Elisp
;; interpreter space.
;;
;;; Code:

(require 'files)

(defgroup vex-util nil
  "Vex utilities."
  :group 'extensions
  :group 'convenience)

(defcustom vex-opacity-prompt "Opacity: "
  "Opacity prompt string."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-opacity .9
  "Opacity default value.
The opacity value is a number from 0 to 1,
with zero being fully transparent and 1 being fully opaque."
  :type 'float
  :group 'vex-util
  :safe t)

(defcustom vex-transset "transset"
  "Transset is a simple file for X servers.
It lets the user set the transparency on a window."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-transset-options "-a"
  "Options for the `vex-transset' program."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-viewer "feh"
  "An Image viewer program."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-viewer-prompt "Image file: "
  "Image prompt string."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-viewer-args-prompt "Args: "
  "Image viewer args prompt string."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-viewer-options "--bg-fill"
  "Options for the `vex-image-viewer' program."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-viewer-args "-g +0-0"
  "An Image viewer default args."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-dir (expand-file-name "~/media/images/wallpapers")
  "Options for the `vex-image-viewer' program."
  :type 'string
  :group 'vex-util
  :safe t)

;;;###autoload
(defun set-frame-transparency (&optional opacity)
  "Set OPACITY transparency in current frame."
  (interactive)
  (let ((opacity (or opacity
                     (read-number vex-opacity-prompt vex-opacity))))
    (if (executable-find vex-transset)
        (async-shell-command
         (format "%s %s %.1f"
                 vex-transset
                 vex-transset-options
                 opacity))
      (message "Program %s not found" vex-transset))))

;;;###autoload
(defun set-wallpaper (image &optional prefix)
  "Set background IMAGE using `vex-image-viewer' binary.
with universal argument PREFIX will prompt a second one,
asking for image viewer complementary args."
  ;; (interactive (list …)) → This is the most general way
  ;; to fill function arguments from user input.
  ;; This list elements will be passed as arguments to your function.
  ;; Usually, it's like this (interactive some_lisp_code) where some_lisp_code
  ;; evaluates to a list.
  (interactive
   (list
    ;; get image file
    (read-file-name vex-image-viewer-prompt
                    vex-image-dir
                    nil
                    t)
    ;; get current prefix argument (universal argument)
    ;; optional!
    current-prefix-arg))
  ;; body:
  ;; get args if C-u (universal argument) was used
  (let ((args (if prefix
                  (read-string vex-image-viewer-args-prompt
                               vex-image-viewer-args))))
    ;; let body:
    ;; conditions (switch/case equivalent)
    (cond
     ;; it is possible to execute vex-image-viewer program
     ((not (executable-find vex-image-viewer))
      (message "Program %s not executable" vex-image-viewer))
     ;; verify if the file exists and it's a regular file
     ((or (file-directory-p image) (not (file-exists-p image)))
      (message "Image file %s not found" image))
     ;; default call vex-image-viewer to set the wallpaper
     (t
      ;; Remember: In Elisp, you will often be better served by
      ;; calling `start-process' directly, since it offers more
      ;; control and does not impose the use of
      ;; a shell (with its need to quote arguments).
      ;; TODO: Research, it is really necessary to use start-process?
      (async-shell-command
       (format "%s %s %s %s"
               vex-image-viewer
               vex-image-viewer-options
               args
               image))))))

;;;###autoload
(defun execute-file (file &optional prefix)
  "Execute arbitrary FILE using `start-process'.
If PREFIX \\[universal-argument] was used,
display a secondary prompt for additional arguments."
  ;; f - a valid file, P - raw prefix
  (interactive "fFile: \nP")
  (let* (
         ;; set only the file name (remove full path)
         (name (file-name-nondirectory file))
         ;; set default directory
         (default-directory (file-name-directory file))
         ;; use a pipe, or t to use a pty
         (process-connection-type t)
         ;; if prefix set arguments string
         (args (if prefix
                   (split-string (read-string "Args: ")))))
    (cond
     ;; test if file is a directory
     ((file-directory-p file)
      (message "Directories are not executable files"))
     ;; test if its not possible to access the directory
     ((not (file-accessible-directory-p default-directory))
      (message "Directory not accessible"))
     ;; test if the file is a executable
     ((file-executable-p file)
      (apply 'start-process name (concat "*" name "*") file args))
     ;; default, not a executable file
     (t (message "File %s is not executable" name)))))

(provide 'vex-util)
;;; vex-util.el ends here
