;;; lex-term.el --- term mode related functions/commands -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
;; Version: 0.0.3 Alpha
;; Keywords: term terminal emulator
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
;; This library provides little hacks for the terminal emulator.
;;
;;; Code:

(require 'term)

(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'term)

(defcustom term-bind-key-alist
  '(("C-c C-c" . term-interrupt-subjob)
    ("C-c C-e" . term-send-esc)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-return)
    ("C-y" . term-paste)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("<C-backspace>" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-d" . term-send-delete-word)
    ("M-," . term-send-raw)
    ("M-." . comint-dynamic-complete))
  "The key alist that will rebounded.
If you do not like default setup, modify it, with (KEY . COMMAND)."
  :type 'alist
  :group 'term)

;;;###autoload
(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

;;;###autoload
(defun term-send-return ()
  "Use term-send-raw-string \"\C-m\" instead term-send-input.
Because term-send-input have bug that will duplicate input when you C-a and C-m in terminal."
  (interactive)
  (term-send-raw-string "\C-m"))

;;;###autoload
(defun term-send-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

;;;###autoload
(defun term-send-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

;;;###autoload
(defun term-send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

;;;###autoload
(defun term-send-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

;;;###autoload
(defun term-send-reverse-search-history ()
  "Search history reverse."
  (interactive)
  (term-send-raw-string "\C-r"))

;;;###autoload
(defun term-send-delete-word ()
  "Delete word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

;;;###autoload
(defun term-send-quote ()
  "Quote the next character in term-mode.
Similar to how `quoted-insert' works in a regular buffer."
  (interactive)
  (term-send-raw-string "\C-v"))

;;;###autoload
(defun term-send-M-x ()
  "Type M-x in term-mode."
  (interactive)
  (term-send-raw-string "\ex"))

;;;###autoload
(defun term-setup-keystroke ()
  "Keystroke setup of `term-char-mode'.

By default, the key bindings of `term-char-mode' conflict with user's keystroke.
So this function unbinds some keys with `term-raw-map',
and binds some keystroke with `term-raw-map'."

  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (cl-dolist (unbind-key term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; Add some i use keys.
    ;; If you don't like my keystroke,
    ;; just modified `term-bind-key-alist'
    (cl-dolist (element term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))

;;;###autoload
(defun term-kill-buffer-hook ()
  "Function to be added in the `kill-buffer-hook' list."
  ;; get current buffer (the buffer being killed)
  ;; get the current buffer associate process
  (let* ((buffer (current-buffer))
         (process (get-buffer-process buffer)))
    ;; verify if term is the major mode from the current buffer (implicit)
    (if (not (eq major-mode 'term-mode)) nil
      ;; if the process is active quit it, otherwise
      ;; do nothing: witch means just kill the buffer (continue)
      (when process (term-quit-subjob)))))

;;;###autoload
(defun open-terminal (name)
  "Call `make-term' with the right arguments.
Asks for the NAME of the created terminal buffer interactively.
Get shell from the SHELL environment variable directly."
  (interactive "sBuffer-name: ")
  (let* ((name (if (string-empty-p name) "terminal" name))
         (buffer (make-term name (getenv "SHELL"))))
    (if (not (buffer-live-p buffer)) nil
      (set-buffer buffer)
      ;; verify if term-mode and term-char-mode are available
      (when (and (fboundp 'term-mode)
                 (fboundp 'term-char-mode))
        (funcall 'term-mode)
        (funcall 'term-char-mode))
      ;; switch to the term buffer
      (switch-to-buffer buffer))))

(provide 'lex-term)

;;; lex-term.el ends here
