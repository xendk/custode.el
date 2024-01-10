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

(defvar custode--project-states '()
  "State storage for tasks.

The format is:
((\"project_root\" . (
    (:running . 0)
)))
")

(defvar custode-lighter
  '(:eval
    (when (and custode-mode
               (custode--get-current-project-tasks))
      (let* ((project-state (custode--get-project-state (custode--current-project-root)))
             (running (cdr (assoc :running (cdr project-state)))))
        (if (and (numberp running) (> running 0))
            (propertize "👁" 'face 'compilation-mode-line-run)
          "👁"))))
  "Mode line lighter for Custode.

The value of this variable is a mode line template as in
`mode-line-format'.")

(put 'custode-lighter 'risky-local-variable t)

(defun custode-add-task (task command)
  "Add a task to the current project.

TASK is the name of the task, COMMAND is the command to run."
  (interactive "sTask: \nsCommand: ")
  (let ((project-root (custode--current-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (unless (custode--get-project project-root)
      (push (cons project-root '()) custode--tasks))
    (let ((project (custode--get-project project-root)))
      (push (cons task (list (cons :task command))) (cdr project)))))

;; TODO: These two could further limit the task list to
;; enabled/disabled tasks.
(defun custode-enable-task (task-name)
  "Enable a task."
  (interactive
   (list
    (if (custode--current-project-root)
        (completing-read "Task: " (custode--get-current-project-tasks) nil t)
      (user-error "Not in a project"))))
  (let ((project-root (custode--current-project-root)))
    (custode--set-task-active project-root task-name t)))

(defun custode-disable-task (task-name)
  "Disable a task."
  (interactive
   (list
    (if (custode--current-project-root)
        (completing-read "Task: " (custode--get-current-project-tasks) nil t)
      (user-error "Not in a project"))))
  (let ((project-root (custode--current-project-root)))
    (custode--set-task-active project-root task-name nil)))

;;;###autoload
(define-minor-mode custode-mode
  "Minor mode for running tasks on file save."
  :lighter custode-lighter
  :group 'custode
  :init-value nil
  (if custode-mode
      (add-hook 'after-save-hook 'custode--after-save-hook t t)
    (remove-hook 'after-save-hook 'custode--after-save-hook t)))

;;;###autoload
(define-globalized-minor-mode global-custode-mode custode-mode
  custode-mode :group 'custode)

(define-compilation-mode custode-task-mode "Custode"
  "Major mode for custode tasks."
  ;; Kill the "Custode finished" message that compilation-handle-exit outputs.
  (setq-local inhibit-message t)
  ;; Add our finish function to handle things when the process ends.
  (setq-local compilation-finish-functions #'custode--compilation-finish-function))

(defun custode--compilation-finish-function (buffer outstr)
  "Finish handler for custode-task-mode.

BUFFER is the process buffer, OUTSTR is compilation-mode's result string."
  (let ((project-state (custode--get-project-state (custode--current-project-root))))
    (when (assoc :running (cdr project-state))
      (setcdr (assoc :running (cdr project-state))
              (1- (cdr (assoc :running (cdr project-state)))))))
  (force-mode-line-update t)
  (if (string-match "finished" outstr)
      ;; Remove the buffer window if succesful.
      (when (get-buffer-window buffer t)
        ;; Unless it's the only window in the frame.
        (unless (< (length (window-list nil (get-buffer-window buffer))) 2)
          (delete-window (get-buffer-window buffer))))
    (unless (get-buffer-window buffer t)
      ;; Display buffer if task failed.
      (display-buffer buffer))))

(defun custode--after-save-hook ()
  "After save hook for custode-mode.

Triggers running active tasks if the file is in a project."
  (let ((project-root (custode--current-project-root)))
    (when project-root
      (let ((tasks (custode--get-active-tasks project-root))
            (default-directory project-root))
        (dolist (task tasks)
          (custode--start project-root (car task) (car (cdr task))))))))

(defun custode--current-project-root ()
  "Get the project root of the current project, or nil if no project."
  (let ((current-project (project-current)))
    (when current-project
      (project-root current-project))))

(defun custode--get-task-state (project-root task)
  "Returns task state for PROJECT-ROOT and TASK.

Creates the state if not found."
  (let ((key (concat project-root "\0" task)))
    (unless (assoc key custode--task-states)
      (let ((val (copy-tree '())))
        (push (cons key val) custode--task-states)))
    (assoc key custode--task-states)))

(defun custode--get-project-state (project-root)
  "Returns project state for PROJECT-ROOT.

Creates the state if not found."
  (unless (assoc project-root custode--project-states)
    (let ((val (copy-tree '())))
      (push (cons project-root val) custode--project-states)))
  (assoc project-root custode--project-states))

(defun custode--get-project (project-root)
  "Get PROJECT-ROOT project.

Returns (PROJECT-ROOT . TASKS) or nil if not found."
  (assoc project-root custode--tasks))

(defun custode--get-current-project-tasks ()
  "Get tasks of current project."
  (let ((project-root (custode--current-project-root)))
    (when project-root
      (cdr (custode--get-project project-root)))))

(defun custode--set-task-active (project-root task-name state)
  "Set task `active' state.

PROJECT-ROOT is the project root, TASK-NAME is the task name and state
is the state."
  (let* ((project-tasks (or (cdr (custode--get-project project-root))
                            (error "Unknown project %s" project-root)))
         (task (or (alist-get task-name project-tasks nil nil 'equal)
                   (error "Unknown task %s" task-name)))
         (task-state (custode--get-task-state project-root task-name)))
    (if (assoc :active (cdr task-state))
        (setf (cdr (assoc :active (cdr task-state))) state)
      (push (cons :active state) (cdr task-state)))))

(defun custode--get-active-tasks (project-root)
  "Get active tasks for PROJECT-ROOT.

Returns a list of (task-name task-command)."
  (let ((project-tasks (alist-get project-root custode--tasks nil nil 'equal))
        (active-tasks (list)))
    (if project-tasks
        (mapcar (lambda (elem)
                  (when (alist-get :active (cdr (custode--get-task-state project-root (car elem))))
                    (push (list (car elem) (alist-get :task (cdr elem))) active-tasks))
                  ) project-tasks))
    active-tasks))

(defun custode--start (project-root task command)
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
             ;; Bind `compilation-in-progress' so compile will add our
             ;; process here, so we can throw it away. If our process
             ;; gets added to the real `compilation-in-progress',
             ;; it'll trigger a "[Compiling]" lighter in the
             ;; mode-line, ond we don't want that.
             (compilation-in-progress nil)
             ;; Tell Emacs that it's OK to kill the process without asking.
             (compilation-always-kill t))
        ;; `compile' attempts to save buffers, so we'll
        ;; go directly to `compilation-start'.
        (compilation-start command 'custode-task-mode)
        (let ((project-state (custode--get-project-state project-root)))
          (if (assoc :running (cdr project-state))
              (setcdr (assoc :running (cdr project-state))
                      (1+ (cdr (assoc :running (cdr project-state)))))
            (push (cons :running 1) (cdr project-state))))
        (force-mode-line-update t)))))

(provide 'custode)
;;; custode.el ends here
