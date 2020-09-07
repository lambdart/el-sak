;;; lex.el --- vanilla Emacs extensions -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Version: 0.0.2 Alpha
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
;; This library extends other libraries (adds more (clever) functions to it),
;; this functions should be small in code size and its goals are to engage
;; the overall usability, some of them has the prefix 'safe' which indicates
;; their purposed to be a more safer version compered to the
;; original counterparts.
;;
;; Others don't have any prefix and this is intentional,
;; as they are basic functions and will be called from the
;; interactive prompt (minibuffer) or keybinded latter.
;;
;; The functions names follows the code convection:
;; ([prefix-]action-target-object).
;;
;;; Code:

(require 'delsel)
(require 'files)
(require 'simple)
(require 'recentf)
(require 'completion)
(require 'replace)
(require 'compile)
(require 'cl-seq)
(require 'subr-x)

(eval-when-compile
  (require 'cl-macs))

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

(defun safe-kill-buffer (buffer-or-name)
  "Kill buffer specified by BUFFER-OR-NAME, if exists."
  (unless (or (stringp buffer-or-name)
              (bufferp buffer-or-name))
    (error "`buffer-or-name' must be a string or a buffer object"))
  (let ((buffer (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

;;;###autoload
(defun safe-funcall (func args)
  "Call FUNC with ARGS, if it's bounded."
  (when (fboundp func)
    (apply 'funcall func args)))

;;;###autoload
(defun safe-mkdir (dir)
  "Create DIR in the file system."
  (when (and (not (file-exists-p dir))
             (make-directory dir :parents))))

;;;###autoload
(defun safe-start-process (name program args)
  "Just a `start-process' function wrapper.
The program will be stated if exists in \\[exec-path]."
  (if (executable-find program)
      (start-process name nil program args)
    (message "Unable to start %s program, executable not found" program)))

;;;###autoload
(defun safe-set-frame-font (font)
  "Set the default font to FONT."
  (cond ((find-font (font-spec :name font))
         (set-frame-font font nil t))))

;;;###autoload
(defun goto-minibuffer-window ()
  "Go to the active minibuffer, if available.

Bind this to `completion-list-mode-map' to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer."

  (interactive)
  (let ((window (active-minibuffer-window)))
    (when window
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
(defun compile-at-dir (dir command)
  "Compile passing COMMAND at DIR.
Just a `compile' function wrapper."
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (compile command))))

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
  (save-mark-and-excursion
    (let ((beg (line-beginning-position))
          (end (line-end-position))
          (i (or n 1)))
      (when (region-active-p)
        ;; update region positions
        (setq beg (region-beginning)
              end (region-end)))
      (let ((region (buffer-substring beg end)))
        (while (> i 0)
          (goto-char (line-end-position))
          (newline)
          (insert region)
          (setq i (1- i)))))))

;; restore region if necessary
;; (when (region-active-p)
;;   (goto-char beg)
;;   (push-mark end)
;;   (setq mark-active t))))

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
(defun kill-region-or-backward-word ()
  "Kill region or `backward-kill-word'."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

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
   ;; if char isn't a space or tab: complete
   ((looking-at "\\_>")
    (complete nil))
   ;; default: indent
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
   ;; if minibuffer active: unless a valid completion:
   ;; complete-symbol
   ((minibufferp)
    (unless (minibuffer-complete)
      (complete-symbol nil)))
   ;; if mark is active: indent region
   (mark-active
    (indent-region (region-beginning) (region-end)))
   ;; if char isn't a space or tab: complete
   ((looking-at "\\_>")
    (complete-symbol nil))
   ;; default: indent
   (t (indent-according-to-mode))))

;;;###autoload
(defun switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*scratch*"))) ; scratch buffer or create
    ;; write default message inside scratch buffer
    (with-current-buffer "*scratch*"
      ;; if buffer size is zero indicates new buffer
      (when (zerop (buffer-size))
        (insert (substitute-command-keys initial-scratch-message)))
      ;; change mode to the initial-major-mode of choice
      (if (eq major-mode 'fundamental-mode)
          (funcall initial-major-mode)))
    ;; finally switch to scratch buffer
    (switch-to-buffer buffer)))

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
(defun force-minibuffer-exit ()
  "Force `minibuffer' to exit."
  (interactive)
  (let ((minibuffer (active-minibuffer-window)))
    (when minibuffer
      (select-window minibuffer)
      (funcall 'minibuffer-keyboard-quit))))

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

;;;###autoload
(defun describe-symbol-at-point ()
  "Describe symbol at point."
  (interactive)
  (let ((symbol (symbol-at-point)))
    (when symbol
      (describe-symbol symbol))))

(defun recentf-candidates ()
  "Get recent files candidates."
  (mapcar 'abbreviate-file-name recentf-list))

;;;###autoload
(defun recentf-find-file ()
  "Find recent file."
  (interactive)
  (let ((candidate
         (completing-read "Recentf: "
                          (recentf-candidates) nil t)))
    ;; find files
    (find-file candidate)))

;;;###autoload
(defun add-dir-to-load-path (dir)
  "Add arbitrary DIR (string) to `load-path'.
If call interactively asks for the directory using
the \\[minibuffer]."
  ;; interactively maps DIR argument.
  (interactive
   (list
    (expand-file-name
     (read-directory-name
      "Dir:" (concat user-emacs-directory "site-lisp/") nil 'confirm))))
  ;; body:
  (let (
        ;; verify if the dir comes from the
        ;; interactive form which returns a (cons) list)
        (dir (if (equal (type-of dir) 'cons)
                 (car dir)
               dir)))
    ;; remove last / (if necessary)
    (setq dir (replace-regexp-in-string "\/$" "" dir))
    ;; switch/case
    (cond
     ;; verify if directory exists
     ((not (file-exists-p dir))
      (message "Directory not found"))
     ;; verify if it's already a member
     ((member dir load-path)
      (message "Already present"))
     ;; finally push (add) and logs
     (t
      (push dir load-path)
      (message "Dir %s added to load-path" dir)))))

;; TODO: (defun rm-dir-from-load-path (&optional dir) (interactive))
;; TODO: (defun clean-load-path-duplicates () (interactive))

;;;###autoload
(defun compile-history ()
  "Compile using `compile-history' as candidates."
  (interactive)
  (let* ((candidates compile-history)
         (compile-command
          (completing-read "Command: "
                           candidates nil
                           'confirm
                           ""
                           `(compile-history))))
    (when (not (string-empty-p compile-command))
      (compile compile-command))))

(defun command-history-candidates ()
  "Return \\[command-history] string candidates."
  (let* ((size (length command-history))
         (commands (split-string (prin1-to-string command-history) ") "))
         (command (nth 0 commands))
         (candidates nil)
         (i 1))
    ;; process first one
    (setq command (concat (substring command 1 (length command)) ")"))
    (setf candidates (cons command candidates))
    ;; process the 'middle' ones
    (while (< i size)
      (setq command (concat (nth i commands) ")"))
      (setf candidates (cons command candidates))
      (setq i (+ i 1)))
    ;; process last one
    (setq command (nth (- size 1) commands)
          command (substring command 0 (- (length command) 1)))
    (setf candidates (cons command candidates))
    candidates))

;;;###autoload
(defun execute-command-history ()
  "Execute a command from `command-history-candidates'."
  (interactive)
  (let ((command
         (completing-read
          "C-x C-c "
          (command-history-candidates) nil 'corfirm "(")))
    (save-restriction
      (eval (read command)))))

(defun parse-mark-line-to-string (pos)
  "Return line string at position POS."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((line (car (split-string (thing-at-point 'line) "[\n\r]"))))
      (remove-text-properties 0 (length line) '(read-only) line)
      (if (string= "" line)
          "<EMPTY LINE>"
        line))))

(defun mark-ring-candidates ()
  "Return parsed mark ring candidates."
  (cl-loop with marks = (if (mark t)
                            (cons (mark-marker) mark-ring)
                          mark-ring)
           for mark in marks
           with max-line-number = (line-number-at-pos (point-max))
           with width = (length (number-to-string max-line-number))
           for m = (format (concat "%" (number-to-string width) "d: %s")
                           (line-number-at-pos mark)
                           (parse-mark-line-to-string mark))
           unless (and recip (assoc m recip))
           collect (cons m mark) into recip
           finally return recip))

;;;###autoload
(defun mark-goto-char ()
  "Browse `mark-ring' interactively and jump to the selected position."
  (interactive)
  (let ((candidates (mark-ring-candidates))
        (position nil))
    (cond
     ;; if not candidates nothing to do, logs
     ;; and leave
     ((not candidates)
      (message "Mark ring is empty"))
     ;; else goto position
     (t
      (setq position
            (completing-read "Goto: " candidates nil t))
      (goto-char (cdr (assoc position candidates)))))))

(provide 'lex)
;;; lex.el ends here
