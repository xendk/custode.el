;;; custode.el --- File watching with an Emacs twist  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@xen.dk>
;; Keywords: tools
;; Package-Requires: ((emacs "28"))
;; Package-Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'compile)
(require 'project)

(define-compilation-mode custode-task-mode "Custode"
  "Major mode for custode tasks."
  ;; Kill the "Custode finished" message that compilation-handle-exit outputs.
  (setq-local inhibit-message t)
  ;; Add our finish function to handle things when the process ends.
  (setq-local compilation-finish-functions #'custode-compilation-finish-function))

(defun custode-compilation-finish-function (buffer outstr)
  "Finish handler for custode-task-mode.

BUFFER is the process buffer, OUTSTR is compilation-mode's result string."
  (unless (string-match "finished" outstr)
    ;; Display buffer if task failed.
    (display-buffer buffer)))

(defun custode-start (command)
  "Work in progress."
  (interactive)
  (let* (;; We need different buffers per project/task.
         (buffer-name-func #'(lambda (name-of-mode)
                               (concat "*" (downcase name-of-mode) " test*"))))
    ;; todo: deal with buffer existence.
    (unless (get-buffer (funcall buffer-name-func "custode-task-mode"))
      (let* (;; Don't show the buffer per default.
             (display-buffer-overriding-action '(display-buffer-no-window))
             (compilation-buffer-name-function buffer-name-func)
             (buffer (progn
                       ;; `compile' attempts to save buffers, so we'll
                       ;; go directly to `compilation-start'.
                       (compilation-start command 'custode-task-mode)))
             (comp-proc (get-buffer-process buffer)))
        ;; Tell emacs that it's OK to kill the process without asking.
        (set-process-query-on-exit-flag comp-proc nil)))))

;; (custode-start "sleep 1 && echo \"done\" && false")

(provide 'custode)
;;; custode.el ends here
