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

(provide 'vex-window)
;; vex-window.el ends here
