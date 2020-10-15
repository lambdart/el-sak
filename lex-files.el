;;; lex-files.el --- files related functions/commands -*- lexical-binding: t -*-
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

(require 'files)
(require 'recentf)
(require 'loadhist)

;;;###autoload
(defun file-type (filename &optional arg)
  "Show file type information at echo area."
  (interactive "fFile: \np")
  (let ((buffer (get-buffer-create "*File Type*"))
        (command
         (format "file \"%s\""
                 (expand-file-name filename))))
    (shell-command command (when arg buffer))))

(defun recent-file-candidates ()
  "Return recent files candidates."
  (mapcar 'abbreviate-file-name recentf-list))

;;;###autoload
(defun find-recent-file ()
  "Find recent file."
  (interactive)
  (let ((file
         (completing-read "Find recent file: "
                          (recent-file-candidates) nil t)))
    ;; find files
    (find-file file)))

;;;###autoload
(defun execute-file (executable &optional args)
  "Execute arbitrary EXECUTABLE file using `start-process'.

If \\[universal-argument] is used, display a secondary
prompt asking for additional ARGS - arguments."

  (interactive
   (list
    ;; get executable file
    (read-file-name "File: " nil nil t)
    ;; get arguments, if prefix - \\[universal-argument] - was used
    (if current-prefix-arg
        (read-string "Args: "))))
  (let*
      ;; set only the file name (remove full path)
      ((name (file-name-nondirectory executable))
       ;; set default directory
       (default-directory (file-name-directory executable))
       ;; use a pipe, or t to use a pty
       (process-connection-type t))
    (cond
     ;; test if file is a directory
     ((file-directory-p executable)
      (message "Directories are not executable files"))
     ;; test if its not possible to access the directory
     ((not (file-accessible-directory-p default-directory))
      (message "Directory not accessible"))
     ;; test if the file is a executable
     ((file-executable-p executable)
      (apply 'start-process name nil executable args))
     ;; default, not a executable file
     (t (message "File %s is not executable" name)))))

;;;###autoload
(defun delete-file-at-point ()
  "Delete file at point (filename or region)."
  (interactive)
  (let ;; get file at point
      ((file (thing-at-point 'filename t)))
    ;; switch/case equivalent
    (cond
     ;; case no file found: nothing to do,
     ;; display a message and leave
     ((eq file nil)
      (message "No file at point was found"))
     ;; case: file is a directory
     ((file-directory-p file)
      ;; deleting directories can be dangerous
      ;; don't it at point
      (message "File is a directory, use `delete-directory'"))
     ;; case: file exist: delete it (asks for confirmation)
     ((file-exists-p file)
      (if (not (y-or-n-p (format "Delete file: %s ? " file)))
          ;; no, just clean echo area
          (message "")
        ;; else, (yes): delete file and display message
        (delete-file file)))
     ;; default: call delete-file interactively
     (t (message "File does not exists")))))

(provide 'lex-files)

;;; lex-files.el ends here

