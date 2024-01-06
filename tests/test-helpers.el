;; For match-buffers.
(require 'compat-29)

(defun wait-for-custode-tasks (&optional delay)
  "Wait for custode tasks to finish."
  (setq delay (or delay 0.01))
  (let ((buffers (match-buffers "*custode")))
    (while (seq-some
            (lambda (buffer)
              (let ((comp-proc (get-buffer-process buffer)))
                (and comp-proc (eq (process-status comp-proc) 'run))))
            buffers)
      (sit-for delay))))
