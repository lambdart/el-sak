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

(defcustom vex-opacity .8
  "Opacity default value.
The opacity value is a number from 0 to 1,
with zero being fully transparent and 1 being fully opaque."
  :type 'float
  :group 'vex-util
  :safe t)

(defcustom vex-transset "transset"
  "Transset is a simple program for X servers.
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

(defcustom vex-image-prompt "Image file: "
  "Image prompt string."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-viewer-options "--bg-fill"
  "Options for the `vex-image-viewer' program."
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
  (interactive "P")
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
(defun set-wallpaper (&optional image-file)
  "Set IMAGE-FILE as background/wallpaper using `vex-image-viewer' binary."
  (interactive "P")
  (let ((image-file (or image-file
                        (read-file-name vex-image-prompt
                                        vex-image-dir
                                        nil
                                        'confirm))))
    (if (not (executable-find vex-image-viewer))
        (message "Program %s not found" vex-image-viewer)
      (async-shell-command
       (format "%s  %s  %s"
               vex-image-viewer
               vex-image-viewer-options
               image-file)))))

(provide 'vex-util)
;;; vex-util.el ends here
