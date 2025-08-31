;;; -*- lexical-binding: t; -*-
;; For match-buffers.
(require 'compat)

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
