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

(defvar custode--tasks '()
  "List of known tasks across all projects.

The format is:
((\"project_root\" . (
  \"task1\" (
    (:task . \"eldev test\")
    )
  )
))
")

(defvar custode--task-states '()
  "State storage for tasks.

The format is:
((\"project_root\\0task\" . (
    (:active . t)
)))
")

(defvar custode-lighter '(:eval (when (and custode-mode
                                           (custode--get-current-project-tasks)) "👁"))
  "Mode line lighter for Custode.

The value of this variable is a mode line template as in
`mode-line-format'.")

(put 'custode-lighter 'risky-local-variable t)

(define-minor-mode custode-mode
  "Minor mode for running tasks on file save."
  :global t
  :lighter custode-lighter
  :group 'custode
  :init-value nil
  (if custode-mode
      (add-hook 'after-save-hook 'custode-after-save-hook t t)
    (remove-hook 'after-save-hook 'custode-after-save-hook t)))

(defun custode-after-save-hook ()
  (let ((current-project (project-current)))
    (when current-project
      (let ((tasks (custode--get-active-tasks (project-root current-project)))
            (default-directory (project-root (project-current t))))
        (dolist (task tasks)
          (message "task %s" (car task))
          (message "command %s" (cdr task))
          (custode--start (car task) (car (cdr task)))
          )
        ))))

(define-compilation-mode custode-task-mode "Custode"
  "Major mode for custode tasks."
  ;; Kill the "Custode finished" message that compilation-handle-exit outputs.
  (setq-local inhibit-message t)
  ;; Add our finish function to handle things when the process ends.
  (setq-local compilation-finish-functions #'custode--compilation-finish-function))

(defun custode--compilation-finish-function (buffer outstr)
  "Finish handler for custode-task-mode.

BUFFER is the process buffer, OUTSTR is compilation-mode's result string."
  (if (string-match "finished" outstr)
      ;; Remove the buffer window if succesful.
      (when (get-buffer-window buffer t)
        (delete-window (get-buffer-window buffer)))
    (unless (get-buffer-window buffer t)
      ;; Display buffer if task failed.
      (display-buffer buffer))))

(defun custode--get-task-state (project-root task)
  "Returns task state for PROJECT-ROOT and TASK.

Creates the state if not found."
  (let ((key (concat project-root "\0" task)))
    (unless (assoc key custode--task-states)
      (let ((val (copy-tree '((:active . nil)))))
        (push (cons key (copy-tree '((:active . nil)))) custode--task-states)))
    (cdr (assoc key custode--task-states))))

(defun custode--get-tasks (project-root)
  "Get project tasks.

PROJECT-ROOT is the path to the root of the project."
  (alist-get project-root custode--tasks nil nil 'equal))

(defun custode--get-current-project-tasks ()
  "Get tasks of current project."
  (let ((current-project (project-current)))
    (when current-project
      (custode--get-tasks (project-root current-project)))))

(defun custode-enable-task (project task-name)
  "Enable a task."
  (custode--set-task-active project task-name t))

(defun custode-disable-task (project task-name)
  "Disable a task."
  (custode--set-task-active project task-name nil))

(defun custode--set-task-active (project task-name state)
  "Set task `active' state.

PROJECT is the project root, TASK-NAME is the task name and state
is the state."
  (let* ((project-tasks (or (custode--get-tasks project)
                            (error "Unknown project %s" project)))
         (task (or (alist-get task-name project-tasks nil nil 'equal)
                   (error "Unknown task %s" task-name)))
         (task-state (custode--get-task-state project task-name)))
    (setf (alist-get :active task-state) state)))

(defun custode--get-active-tasks (project)
  "Get active tasks for PROJECT.

Returns a list of (task-name task-command)."
  (let ((project-tasks (alist-get project custode--tasks nil nil 'equal))
        (active-tasks (list)))
    (if project-tasks
        (mapcar (lambda (elem)
                  (when (alist-get :active (custode--get-task-state project (car elem)))
                    (push (list (car elem) (alist-get :task (cdr elem))) active-tasks))
                  ) project-tasks))
    active-tasks))

(defun custode--start (task command)
  "Start TASK with COMMAND."
  (interactive)
  (let* (;; We need different buffers per project/task.
         (buffer-name-func #'(lambda (name-of-mode)
                               (concat "*" (downcase name-of-mode) " "
                                       default-directory " " task "*"))))
    ;; todo: deal with buffer existence.
    (unless (get-buffer (funcall buffer-name-func "custode-task-mode"))
      (let* (;; Don't show the buffer per default.
             (display-buffer-overriding-action '(display-buffer-no-window))
             (compilation-buffer-name-function buffer-name-func)
             ;; `compile' attempts to save buffers, so we'll
             ;; go directly to `compilation-start'.
             (buffer (compilation-start command 'custode-task-mode))
             (comp-proc (get-buffer-process buffer)))
        ;; Tell emacs that it's OK to kill the process without asking.
        (set-process-query-on-exit-flag comp-proc nil)))))

;; (custode-start "sleep 1 && echo \"done\" && false")

(provide 'custode)
;;; custode.el ends here
