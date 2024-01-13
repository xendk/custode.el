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
    (:enabled . t)
)))
")

(defvar custode--project-states '()
  "State storage for tasks.

The format is:
((\"project_root\" . (
    (:running . 0)
)))
")

(defvar custode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'custode-add-task)
    (define-key map "e" 'custode-enable-task)
    (define-key map "d" 'custode-disable-task)
    (define-key map "c" 'custode-set-task-args)
    (define-key map "l" 'custode-load)
    (define-key map "s" 'custode-save)
    map)
  "Keymap for custode commands.")

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

(defcustom custode-save-file
  ".custode"
  "The file in the project directory to save tasks in.
This should _not_ be set via .dir-locals.el."
  :group 'custode
  :type 'file)

(defun custode-buffer-name (project-root task-name)
  (concat " *custode " project-root " " task-name "*"))

(defun custode-add-task (task command)
  "Add a task to the current project.

The NAME is purely an identifier, you can use any name you find
appropiate. COMMAND is the command passed to the shell to run.
Initially the command will be disabled, use `custode-enable-task'
to enable it."
  (interactive "sTask: \nsCommand: ")
  (let ((project-root (custode--current-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (let ((project (custode--get-project project-root)))
      (push (cons task (list (cons :task command))) (cdr project)))))

;; TODO: These two could further limit the task list to
;; enabled/disabled tasks.
(defun custode-enable-task (task-name)
  "Enable a task in the current project.

Enabled tasks will automatically run when files in the project is
saved.

Initially, all tasks, whether added with `custode-add-task' or
loaded from `custode-save-file' will be disabled until manually
enabled."
  (interactive
   (list
    (if (custode--current-project-root)
        (completing-read "Task: " (custode--get-current-project-tasks) nil t)
      (user-error "Not in a project"))))
  (let ((project-root (custode--current-project-root)))
    (custode--set-task-enabled project-root task-name t)))

(defun custode-disable-task (task-name)
  "Disable a task in the current project.

Disabled tasks will not be run when project files are saved.

Initially, all tasks, whether added with `custode-add-task' or
loaded from `custode-save-file' will be disabled until manually
enabled."
  (interactive
   (list
    (if (custode--current-project-root)
        (completing-read "Task: " (custode--get-current-project-tasks) nil t)
      (user-error "Not in a project"))))
  (let ((project-root (custode--current-project-root)))
    (custode--set-task-enabled project-root task-name nil)))

(defun custode-load ()
  "Load project tasks from `custode-save-file' file in project root."
  (interactive)
  (let ((project-root (custode--current-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (setcdr (custode--get-project project-root)
            (custode--read-project-tasks project-root))))

(defun custode-save ()
  "Write project tasks to `custode-save-file' file in project root."
  (interactive)
  (let ((project-root (custode--current-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (custode--write-project-tasks project-root (custode--get-current-project-tasks))))

(defun custode-set-task-args (task-name args)
  "Set/unset command arguments for TASK-NAME in the current project.

This is, for instance, useful for temporarily focusing tests on
specific test cases, by supplying the test command with the
appropriate arguments for only running those tests.

The ARGS is a string appended to the shell command for the
task (with a space in between). If the string is empty, revert to
the original task command.

Task arguments persists for the duration of the Emacs session."
  (interactive
   (if (custode--current-project-root)
       (let ((task-name (completing-read "Task: "
                                         (custode--get-current-project-tasks) nil t)))
         (list
          task-name
          (read-string "Task args: "
                       (custode--get-task-args (custode--current-project-root) task-name)
                       'consult-args-history)))
     (user-error "Not in a project")))
  (let ((args (string-trim args))
        (state (custode--get-task-state (custode--current-project-root) task-name)))
    (if (equal args "")
        (setf (cdr state) (assoc-delete-all :args (cdr state)))
      (push (cons :args args) (cdr state)))))

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
  (let ((buffer-window (get-buffer-window buffer t)))
    (if (string-match "finished" outstr)
        ;; Remove the buffer window if succesful.
        (when buffer-window
          ;; Unless it's the only window in the frame.
          (unless (< (length (window-list (window-frame buffer-window))) 2)
            (delete-window buffer-window)))
      (unless buffer-window
        ;; Display buffer if task failed.
        (display-buffer buffer)))))

(defun custode--after-save-hook ()
  "After save hook for custode-mode.

Triggers running enabled tasks if the file is in a project."
  (let ((project-root (custode--current-project-root)))
    (when project-root
      (custode--trigger project-root))))

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

Adds PROJECT-ROOT to the list of projects if not found.

Returns (PROJECT-ROOT . TASKS)."
  (unless (assoc project-root custode--tasks)
    (push (cons project-root '()) custode--tasks)
    (custode-load))
  (assoc project-root custode--tasks))

(defun custode--get-current-project-tasks ()
  "Get tasks of current project."
  (let ((project-root (custode--current-project-root)))
    (when project-root
      (cdr (custode--get-project project-root)))))

(defun custode--set-task-enabled (project-root task-name state)
  "Set tasks enabled' state.

PROJECT-ROOT is the project root, TASK-NAME is the task name and STATE
is `t' or `nil'."
  (let* ((project-tasks (or (cdr (custode--get-project project-root))
                            (error "Unknown project %s" project-root)))
         (task (or (alist-get task-name project-tasks nil nil 'equal)
                   (error "Unknown task %s" task-name)))
         (task-state (custode--get-task-state project-root task-name)))
    (if (assoc :enabled (cdr task-state))
        (setf (cdr (assoc :enabled (cdr task-state))) state)
      (push (cons :enabled state) (cdr task-state)))))

(defun custode--get-enabled-tasks (project-root)
  "Get enabled tasks for PROJECT-ROOT.

Returns a list of task-names."
  (let ((project-tasks (alist-get project-root custode--tasks nil nil 'equal))
        (enabled-tasks (list)))
    (if project-tasks
        (mapcar (lambda (elem)
                  (when (alist-get :enabled (cdr (custode--get-task-state project-root (car elem))))
                    (push (car elem) enabled-tasks))
                  ) project-tasks))
    enabled-tasks))

(defun custode--get-task-args (project-root task-name)
  "Get the currently set args for the PROJECT-ROOT TASK-NAME."
  (let ((state (custode--get-task-state project-root task-name)))
    (cdr (assoc :args (cdr state)))))

(defun custode--trigger (project-root)
  "Trigger tasks on PROJECT-ROOT."
  (let ((tasks (cdr (custode--get-project project-root)))
        (enabled-tasks (custode--get-enabled-tasks project-root))
        (default-directory project-root))
    (dolist (task-name enabled-tasks)
      (let* ((task (cdr (assoc task-name tasks)))
             (command (cdr (assoc :task task)))
             (args (custode--get-task-args project-root task-name)))
        (custode--start project-root task-name
                        (if args
                            (concat command " " args)
                          command))))))

(defun custode--start (project-root task-name command)
  "Start TASK with COMMAND."
  (interactive)
  (let* (;; We need different buffers per project/task.
         (buffer-name (custode-buffer-name project-root task-name))
         (buffer-name-func #'(lambda (name-of-mode)
                               buffer-name)))
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
      (force-mode-line-update t))))

(defun custode--write-project-tasks (project-root tasks)
  "Write project task to `custode-save-file' file in PROJECT-ROOT."
  (let ((filename (concat (file-name-as-directory project-root) custode-save-file)))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp tasks (current-buffer)))
      (write-region nil nil filename nil 'silent))))

(defun custode--read-project-tasks (project-root)
  "Read project tasks from `custode-save-file' file in PROJECT-ROOT."
  (let* ((filename (concat (file-name-as-directory project-root) custode-save-file))
         (read-data (when (file-exists-p filename)
                      (with-temp-buffer
                        (insert-file-contents filename)
                        (read (current-buffer)))))
         (tasks))
    (dolist (item read-data)
      ;; Task name.
      (when (stringp (car item))
        (let ((read-task-name (car item))
              (read-task (cdr item)))
          (when (and (assoc :task read-task)
                     (stringp (cdr (assoc :task read-task))))
            (push (cons read-task-name (list (assoc :task read-task))) tasks)))))
    tasks))
(provide 'custode)
;;; custode.el ends here
