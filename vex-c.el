(defun safe-funcall (func &rest args)
  "Call FUNC with ARGS, if it's bounded."
  (when (fboundp func)
    (funcall args)))

(defun safe-kill-buffer (buffer-or-name)
  "Kill buffer specified by BUFFER-OR-NAME, if exists."
  (unless (or (stringp buffer-or-name)
              (bufferp buffer-or-name))
    (error "`buffer-or-name' must be a string or a buffer object"))
  (let ((buffer (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))



(provide 'vex-c)
;;; vex-c.el ends here
