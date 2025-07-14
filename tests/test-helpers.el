;; For match-buffers.
(when (version< emacs-version "30.0.0")
  ;; Can't remember why I needed this.
  (require 'compat-29))

(defun wait-for-custode-commands (&optional delay)
  "Wait for custode commands to finish."
  (setq delay (or delay 0.01))
  (let ((buffers (match-buffers " *custode")))
    (while (seq-some
            (lambda (buffer)
              (let ((comp-proc (get-buffer-process buffer)))
                (and comp-proc (eq (process-status comp-proc) 'run))))
            buffers)
      (sit-for delay))))
