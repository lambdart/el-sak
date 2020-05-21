(require 'subr)
(require 'files)

(defun safe-start-process (name buffer program &optional args)
  "Start a program in a subprocess."
  (when (executable-find program)
    (start-process name buffer program
      (or args nil))))

(provide 'vex-subr)
;;; vex-subr.el ends here
