;;; vex-util.el --- vanilla Emacs extensions -*- lexical-binding: t -*-
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

(defcustom vex-image-viewer-options "--bg-fill"
  "Options for the `vex-image-viewer' program."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-viewer-args "-g +0+0"
  "An Image viewer default args."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-dir
  (expand-file-name "~/media/images/wallpapers/")
  "Options for the `vex-image-viewer' program."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-image-viewer "feh"
  "An Image viewer program."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-slock "slock"
  "Command line screen locker utility."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-scrot "scrot"
  "Command line screen capture utility."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-scrot-dir
  (expand-file-name "scrot" user-emacs-directory)
  "Where to put captured screen images."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-mixer "mixer"
  "Mixer – set/display soundcard mixer values."
  :type 'string
  :group 'vex-util
  :safe t)

(defcustom vex-mixer-factor 5
  "Volume factor."
  :type 'integer
  :group 'vex-util
  :safe t)

;;;###autoload
(defun set-frame-transparency (&optional opacity)
  "Set OPACITY transparency in current frame."
  (interactive
   (list (read-number "Opacity: " vex-opacity)))
  (if (executable-find vex-transset)
      (async-shell-command
       (format "%s %s %.1f"
               vex-transset
               vex-transset-options
               (or opacity vex-opacity)))
    (message "Program %s not found" vex-transset)))

;;;###autoload
(defun set-wallpaper (image &optional args)
  "Set background IMAGE using `vex-image-viewer'.
With \\[universal-argument]] prompt a secondary one,
asking for image viewer complementary ARGS - arguments."
  ;; (interactive (list …)) → This is the most general way
  ;; to fill function arguments from user input.
  ;; This list elements will be passed as arguments to your function.
  ;; Usually, it's like this (interactive some_lisp_code) where some_lisp_code
  ;; evaluates to a list.
  (interactive
   (list
    ;; get image file
    (read-file-name "Image: "
                    vex-image-dir
                    nil
                    t)

    ;; get current prefix argument (universal argument)
    (if current-prefix-arg
        (read-string "Args: "
                     vex-image-viewer-args))))
  ;; body:
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
             (or args "")
             image)))))

(defun capture-screen (&optional dest)
  "Capture screen (an image) and save at DEST directory."
  (interactive
   (list
    ;; if prefix, read directory name
    (when current-prefix-arg
      (read-directory-name
       "Dir: " nil default-directory t))))
  ;; body:
  (let ((default-directory (expand-file-name
                            (or dest vex-scrot-dir)))
        (scrot (executable-find vex-scrot)))
    (cond
     ;; if screen capture utility
     (scrot
      (funcall 'start-process scrot nil scrot)
      (message "Image saved at %s" default-directory))
     ;; default
     (t
      (message "Command %s not found" vex-scrot)))))

;;;###autoload
(defun lock-screen ()
  "Lock screen using `vex-slock' utility."
  (interactive)
  (if (not (executable-find vex-slock))
      (message "Command %s not found" vex-slock)
    (async-shell-command vex-slock)))

;;;###autoload
(defun set-volume (value &optional direction)
  "Set volume VALUE in up or down DIRECTION."
  (interactive
   (list
    (read-number "Volume: " 50)))
  (let* ((mixer (executable-find vex-mixer))
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
  (let ((factor (or n vex-mixer-factor)))
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
  (let ((factor (or n vex-mixer-factor)))
    (set-volume factor :down)
    (message "Volume lower: -%d" factor)))

;;;###autoload
(defun mute-audio ()
  "Mute volume."
  (interactive)
  (set-volume 0 :set))

;;autoload
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
      ;; finally execute pdftotext
    (async-shell-command cmd))))

(provide 'vex-util)
;;; vex-util.el ends here
