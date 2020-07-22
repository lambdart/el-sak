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
;; This package adds more (clever) functions to the builtin packages,
;; this functions should be small in code size and its goals are to engage
;; the overall usability, some of them has the prefix 'safe' which indicates
;; their purposed to be a more safer version compered to the
;; original counterparts.
;;
;; Others don't have any prefix and this is intentional,
;; as they are basic functions and will be called from the
;; interactive prompt (minibuffer) or keybinded latter.
;;
;; The functions names follows the
;; code convection (action-object) stated in Elisp tips manuals.
;;
;; TODO: Add elisp manual reference!
;; TODO: List the functions here!
;;
;;; Code:

(require 'files)
(require 'simple)
(require 'completion)
(require 'replace)

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
        (push dir load-path)))))

(defun safe-mkdir (dir)
  "Create DIR in the file system."
  (when (and (not (file-exists-p dir))
             (make-directory dir :parents))))

(defun safe-start-process (name program args)
  "Just a `start-process' function wrapper.
The program will be stated if exists in \\[exec-path]."
  (if (executable-find program)
      (start-process name nil program args)
    (message "Unable to start %s program, executable not found" program)))

;;;###autoload
(defun select-minibuffer-window ()
  "Focus the active minibuffer, if available.

Bind this to `completion-list-mode-map' to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer."

  (interactive)
  (let ((window (active-minibuffer-window)))
    (when window
      (select-window window nil))))

(defun safe-set-frame-font (font)
  "Set the default font to FONT."
  (cond ((find-font (font-spec :name font))
         (set-frame-font font nil t))))

;;;###autoload
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

(defun transpose-region (arg)
  "Transpose region by one line, ARG set the direction.
If ARG positive UP otherwise DOWN."
  (unless (region-active-p)
    (error "Reggion is not active"))
  ;; step 1: identifying the text to cut.
  (let ((beg (region-beginning))
        (end (region-end))
        (dir (if (> arg 0) 1 -1))) ;; direction down or up
    ;; step 2: cut and paste
    (let ((text (delete-and-extract-region beg end)))
      (forward-line dir) ;; direction
      (insert text)
      ;; step 3: restore mark if necessary
      (when (region-active-p)
        (setq deactivate-mark nil)
        (set-mark (+ (point) (- beg end)))))))

(defun transpose-line (arg)
  "Transpose line ARG set the direction.
If ARG is positive UP else DOWN."
  ;; step 1: identifying the text to cut.
  (let ((orig (point))
        (beg (line-beginning-position))
        (end (1+ (line-end-position))))
    ;; step 2: cut and paste
    (let ((text (delete-and-extract-region beg end)))
      (forward-line arg)
      (insert text)
      ;; step 3: restore line position
      (forward-char (- orig end)))))

;;;###autoload
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-buffer))))

;;;###autoload
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line or region N times.
If there's no region, the current line will be duplicated.
Otherwise, its lines will be duplicated."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (i (or n 1)))
    (when (region-active-p)
      (setq beg (region-beginning)
            end (region-end)))
    (let ((region (buffer-substring-no-properties beg end)))
      (while (> i 0)
        (goto-char end)
        (newline)
        (insert region)
        (setq i (1- i))))))

;;;###autoload
(defun transpose-lines-up ()
  "Transpose lines in down direction."
  (interactive)
  (if (region-active-p)
      (transpose-region 1)
    (transpose-line 1)))

;;;###autoload
(defun transpose-lines-down ()
  "Transpose lines in up direction."
  (interactive)
  (if (region-active-p)
      (transpose-region -1)
    (transpose-line -1)))

;;;###autoload
(defun transpose-word-left (n)
  "Transpose N words to the opposite direction (left)."
  (interactive "p")
  (transpose-words (- (or n 1))))

;;;###autoload
(defun copy-line (&optional arg)
  "Copy lines, do not kill then.
With prefix argument ARG, kill (copy) that many lines from point."
  (interactive "p")
  (let ((buffer-read-only t)
        (kill-read-only-ok t))
    (save-excursion
      (move-beginning-of-line nil)
      (kill-line arg))))

;;;###autoload
(defun copy-text-or-symbol-at-point ()
  "Get the text in region or symbol at point.
If region is active, return the text in that region.
Else if the point is on a symbol, return that symbol name.
Else return nil."
  (interactive)
  (cond ((use-region-p)
         (buffer-substring-no-properties
          (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties (thing-at-point 'symbol)))
        (t nil)))

;;;###autoload
(defun back-to-indent-or-line (arg)
  "Move point back to indentation or beginning of line.
With argument ARG not nil or 1, move forward ARG - 1 lines first."
  (interactive "p")
  (setq arg (or arg 1))
  ;; first forwards lines
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  ;; back to indentation or beginning if line
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(defun complete-or-indent ()
  "Complete or indent."
  (interactive)
  (cond
   ((looking-at "\\_>") (complete nil))
   (t (indent-according-to-mode))))

;;;###autoload
(defun complete-at-point-or-indent ()
  "This smart tab is a `minibuffer' compliant.

It acts as usual in the `minibuffer'.
Case mark is active, indents region.
Case point is at the end of a symbol, expands it.
Or indents the current line."

  (interactive)
  (cond
   ((minibufferp)
    (unless (minibuffer-complete)
      (complete-symbol nil)))
   (mark-active (indent-region (region-beginning) (region-end)))
   ((looking-at "\\_>") (complete-symbol nil))
   (t (indent-according-to-mode))))

;;;###autoload
(defun switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*scratch*")))
    (with-current-buffer "*scratch*"
      (when (zerop (buffer-size))
        (insert (substitute-command-keys initial-scratch-message)))
      (if (eq major-mode 'fundamental-mode)
          (funcall initial-major-mode)))
    (switch-to-buffer buffer)))

;;;###autoload
(defun byte-compile-current-file ()
  "Save and byte compile current file."
  (interactive)
  (when buffer-file-name
    (save-buffer)
    (byte-compile-file buffer-file-name)))

;;;###autoload
(defun compile-at-dir (dir command)
  "Compile passing COMMAND at DIR.
Just a `compile' function wrapper."
  (if (file-exists-p dir)
      (let ((default-directory dir))
        (compile command))))

;;;###autoload
(defun occur-at-point ()
  "Occur with symbol or region as its arguments."
  (interactive)
  (let*
      ;; get region or symbol
      ((bounds (if (use-region-p)
                   (cons (region-beginning) (region-end))
                 (bounds-of-thing-at-point 'symbol)))
       ;; get string
       (string (unless bounds
                 (read-string "Occur: "))))
    (cond
     ;; region
     (bounds
      (occur (buffer-substring-no-properties
              (car bounds) (cdr bounds)))
      (deactivate-mark))
     ;; default string, symbol
     (t (occur string)))))

(provide 'vex)
;;; vex.el ends here
