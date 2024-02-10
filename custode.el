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

(defvar custode--commands '()
  "List of known commands across all projects.

The format is:
((\"project_root\" . (
  \"eldev test\" (
    (:positioning-function . custode--position-buffer-end)))))
")

(defvar custode--command-states '()
  "Stores runtime state of commands.

The format is:
((\"project_root\\0command\" . (
    (:watching . t)
)))
")

(defvar custode--project-states '()
  "Stores runtime state of projects.

The format is:
((\"project_root\" . (
    (:running . 0)
)))
")

(defvar custode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'custode-add-command)
    (define-key map "k" 'custode-delete-command)
    (define-key map "e" 'custode-watch)
    (define-key map "d" 'custode-unwatch)
    (define-key map "a" 'custode-set-command-args)
    (define-key map "l" 'custode-load)
    (define-key map "s" 'custode-save)
    (define-key map "p" 'custode-set-buffer-positioning)
    map)
  "Keymap for custode commands.")

(defvar custode-lighter
  '(:eval
    (when (and custode-mode
               (custode--get-current-project-commands))
      (let* ((project-state (custode--get-project-state (custode--current-project-root)))
             (running (cdr (assoc :running (cdr project-state)))))
        (if (and (numberp running) (> running 0))
            (propertize " 👁" 'face 'compilation-mode-line-run)
          " 👁"))))
  "Mode line lighter for Custode.

The value of this variable is a mode line template as in
`mode-line-format'.")

(put 'custode-lighter 'risky-local-variable t)

(defvar custode-position-function
  "Function to call to position output buffer.

Automatically set in relevant buffers by custode--start.")

(defcustom custode-save-file
  ".custode"
  "The file in the project directory to save commands in.
This should _not_ be set via .dir-locals.el."
  :group 'custode
  :type 'file)

(defcustom custode-buffer-positioning-functions
  '(custode--position-buffer-end custode--position-buffer-beginning)
  "List of allowed buffer positioning functions.

These functions are called with the buffer as the only argument"
  :group 'custode
  :type '(repeat function))

(defgroup custode-faces nil
  "Faces used by `custode-mode'."
  :group 'custode
  :group 'faces)

(defface custode-completions-args
  '((t :inherit completions-common-part))
  "Face used to highlight command arguments in completion.")

(defun custode-buffer-name (project-root command)
  "Get the buffer name for the COMMAND in PROJECT-ROOT project."
  (concat " *custode " project-root " " command "*"))

(defun custode-add-command (command)
  "Add COMMAND to the current project.

After adding a command, use `custode-watch' to continuously run
it."
  (interactive "sCommand: ")
  (let ((project-root (custode--current-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (let ((project (custode--get-project project-root)))
      (push (cons command '()) (cdr project))
      (message "Created \"%s\"" command))))

(defun custode-delete-command (command)
  "Delete COMMAND from the current project."
  (interactive
   (list
    (custode--completing-read-command "Delete command")))
  (if (yes-or-no-p (format "Really delete \"%s\"? " command))
      (let* ((project-root (custode--current-project-root))
             (project (custode--get-project project-root)))
        (setcdr project (assoc-delete-all command (cdr project)))
        (message "Deleted \"%s\"" command))
    (message "Command not deleted")))

(defun custode-watch (command)
  "Watch COMMAND in the current project.

When watching a command it will automatically be run when files
in the project is saved.

Interactively, the command is run immediately, unless called with
a prefix argument."
  (interactive
   (list
    (custode--completing-read-command "Watch")))
  (let ((project-root (custode--current-project-root)))
    (custode--set-command-watching project-root command t)
    (when (and (called-interactively-p) (not current-prefix-arg))
      (custode--trigger project-root (list command)))
    (message "Watching \"%s\"" command)))

(defun custode-unwatch (command)
  "Stop watching COMMAND in the current project.

Interactively, any output buffer is removed, unless called with a
prefix argument."
  (interactive
   (list
    (custode--completing-read-command "Unwatch")))
  (let ((project-root (custode--current-project-root)))
    (custode--set-command-watching project-root command nil)
    (when (and (called-interactively-p) (not current-prefix-arg))
      (kill-buffer (custode-buffer-name project-root command)))
    (message "Stopped watching \"%s\"" command)))

(defun custode-load ()
  "Load project commands from `custode-save-file' file in project root."
  (interactive)
  (let ((project-root (custode--current-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (setcdr (custode--get-project project-root)
            (custode--read-project-commands project-root))
    (message "Loaded project commands.")))

(defun custode-save ()
  "Write project commands to `custode-save-file' file in project root."
  (interactive)
  (let ((project-root (custode--current-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (custode--write-project-commands project-root (custode--get-current-project-commands))
    (message "Saved project commands")))

(defun custode-set-buffer-positioning (command positioning-function)
  "Set the positioning function for the Custode buffer."
  (interactive
   (list
    (custode--completing-read-command "Set buffer positioning for")
    (intern (completing-read "Positioning function: "
                             (mapcar 'symbol-name custode-buffer-positioning-functions)
                             nil t))))
  (unless (member positioning-function custode-buffer-positioning-functions)
    (error "Unknown positioning function %s" positioning-function))
  (let* ((project-root (custode--current-project-root))
         (project-commands (or (cdr (custode--get-project project-root))
                               (error "Unknown project %s" project-root)))
         (command (assoc command project-commands)))
    (unless command
      (error "Unknown command \"%s\"" command))
    (if (assoc :positioning-function (cdr command))
        (setf (cdr (assoc :positioning-function (cdr command))) positioning-function)
      (push (cons :positioning-function positioning-function) (cdr command)))))

(defun custode-set-command-args (command args)
  "Set/unset command arguments for COMMAND in the current project.

This is, for instance, useful for temporarily focusing tests on
specific test cases, by supplying the test command with the
appropriate arguments for only running those tests.

The ARGS is a string appended to the shell command for the
command (with a space in between). If the string is empty, revert
to the original command without any further arguments.

Interactively, run the command after setting it, if the command
is being watched, unless called with a prefix argument.

Command arguments persists for the duration of the Emacs session."
  (interactive
   (let ((command (custode--completing-read-command "Set arguments for")))
     (list
      command
      (read-string "Command arguments: "
                   (custode--get-command-args (custode--current-project-root) command)
                   'custode-args-history))))
  (let* ((args (string-trim args))
         (project-root (custode--current-project-root))
         (state (custode--get-command-state project-root command)))
    (if (equal args "")
        (setf (cdr state) (assoc-delete-all :args (cdr state)))
      (push (cons :args args) (cdr state)))
    (when (and (called-interactively-p)
               (not current-prefix-arg)
               (custode--command-watching-p project-root command))
      (custode--trigger project-root (list command)))))

;;;###autoload
(define-minor-mode custode-mode
  "Minor mode for running commands on file save."
  :lighter custode-lighter
  :group 'custode
  :init-value nil
  (if custode-mode
      (add-hook 'after-save-hook 'custode--after-save-hook t t)
    (remove-hook 'after-save-hook 'custode--after-save-hook t)))

;;;###autoload
(define-globalized-minor-mode global-custode-mode custode-mode
  custode-mode :group 'custode)

(define-compilation-mode custode-command-mode "Custode"
  "Major mode for custode command output buffers."
  ;; Kill the "Custode finished" message that compilation-handle-exit outputs.
  (setq-local inhibit-message t)
  ;; Add our finish function to handle things when the process ends.
  (setq-local compilation-finish-functions #'custode--compilation-finish-function))

(defun custode--compilation-finish-function (buffer outstr)
  "Finish handler for custode-command-mode.

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
        ;; Display buffer if command failed.
        (display-buffer buffer))
      (funcall custode-position-function buffer))))

(defun custode--position-buffer-end (buffer)
  "Position BUFFER at the end.

Attempts to position the point so the last line is at the bottom
of the window."
  (let* ((window (get-buffer-window buffer t))
         (new-point (save-excursion
                      (with-current-buffer buffer
                        (goto-char (point-max))
                        (vertical-motion (- (floor (/ (window-height window) 2))))
                        (point)))))
    (message "%S" new-point)
    (set-window-point window new-point)))

(defun custode--position-buffer-beginning (buffer)
  "Position BUFFER at the beginning."
  (set-window-point (get-buffer-window buffer t) (point-min)))

(defun custode--after-save-hook ()
  "After save hook for custode-mode.

Triggers running enabled commands if the file is in a project."
  (let ((project-root (custode--current-project-root)))
    (when project-root
      (custode--trigger project-root))))

(defun custode--current-project-root ()
  "Get the project root of the current project, or nil if no project."
  (let ((current-project (project-current)))
    (when current-project
      (project-root current-project))))

(defun custode--get-command-state (project-root command)
  "Returns COMMAND state for PROJECT-ROOT.

Creates the state if not found."
  (let ((key (concat project-root "\0" command)))
    (unless (assoc key custode--command-states)
      (let ((val (copy-tree '())))
        (push (cons key val) custode--command-states)))
    (assoc key custode--command-states)))

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

Returns (PROJECT-ROOT . COMMANDS)."
  (unless (assoc project-root custode--commands)
    (push (cons project-root '()) custode--commands)
    (custode-load))
  (assoc project-root custode--commands))

(defun custode--get-current-project-commands ()
  "Get commands of current project."
  (let ((project-root (custode--current-project-root)))
    (when project-root
      (cdr (custode--get-project project-root)))))

(defun custode--set-command-watching (project-root command state)
  "Set COMMAND watching state.

PROJECT-ROOT is the project root and STATE is `t' or `nil'."
  (let* ((commands (or (cdr (custode--get-project project-root))
                       (error "Unknown project %s" project-root)))
         (command-options (or (assoc command commands)
                              (error "Unknown command \"%s\"" command)))
         (command-state (custode--get-command-state project-root command)))
    (if (assoc :watching (cdr command-state))
        (setf (cdr (assoc :watching (cdr command-state))) state)
      (push (cons :watching state) (cdr command-state)))))

(defun custode--command-watching-p (project-root command)
  "Check whether command is watching."
  (if (member command (custode--get-watching-commands project-root))
      t nil))

(defun custode--get-watching-commands (project-root)
  "Get watching commands for PROJECT-ROOT.

Returns a list of commands."
  (let ((commands (alist-get project-root custode--commands nil nil 'equal))
        (watching (list)))
    (if commands
        (dolist (command commands)
          (when (cdr (assoc :watching (cdr (custode--get-command-state project-root (car command)))))
            (push (car command) watching))))
    watching))

(defun custode--get-command-args (project-root command)
  "Get the currently set arguments for the PROJECT-ROOT COMMAND."
  (let ((state (custode--get-command-state project-root command)))
    (cdr (assoc :args (cdr state)))))

(defun custode--completing-read-command (prompt)
  "Use `completing-read' to read a command in the current project."
  (unless (custode--current-project-root)
    (user-error "Not in a project"))
  (completing-read (concat prompt ": ") #'custode--command-completion-table nil t))

(defun custode--command-completion-table (str pred flag)
  "Completion table for custode commands."
  (pcase flag
    ('metadata '(metadata (category . 'custode-command)
                          (affixation-function . custode--command-completion-table-affixation)))
    (_ (all-completions str (custode--get-current-project-commands) pred))))

(defun custode--command-completion-table-affixation (completions)
  (mapcar (lambda (c)
            (let* ((project-root (custode--current-project-root))
                   (commands (cdr (custode--get-project project-root)))
                   (command (car (assoc c commands)))
                   (args (custode--get-command-args project-root c)))
              (list c
                    (if (custode--command-watching-p project-root command)
                        "✓ " "  ")
                    (when args
                      (propertize (concat " " args) 'face 'custode-completions-args)))))
          completions))

(defun custode--trigger (project-root &optional commands)
  "Trigger commands on PROJECT-ROOT.

Optionally supply COMMANDS to trigger these specific commands,
regardless of whether they're enabled or not."
  (let ((commands (or commands (custode--get-watching-commands project-root)))
        (default-directory project-root))
    (dolist (command commands)
      (let* ((command-options (cdr (assoc command commands)))
             (positioning-function (cdr (assoc :positioning-function command-options)))
             (args (custode--get-command-args project-root command)))
        (custode--start project-root command args positioning-function)))))

(defun custode--start (project-root command &optional args position-function)
  "Start COMMAND in PROJECT-ROOT, optionally with ARGS arguments.

POSITION-FUNCTION is a function that positions the buffer afterwards."
  (interactive)
  (let* (;; We need different buffers per project/command.
         (buffer-name (custode-buffer-name project-root command))
         (buffer-name-func #'(lambda (_name-of-mode)
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
      (let* ((buffer (compilation-start (if args (concat command " " args)
                                          command)
                                        'custode-command-mode))
             (project-state (custode--get-project-state project-root)))
        (if (assoc :running (cdr project-state))
            (setcdr (assoc :running (cdr project-state))
                    (1+ (cdr (assoc :running (cdr project-state)))))
          (push (cons :running 1) (cdr project-state)))
        (with-current-buffer buffer
          (setq-local custode-position-function
                      (or position-function 'custode--position-buffer-beginning))))
      (force-mode-line-update t))))

(defun custode--write-project-commands (project-root commands)
  "Write project COMMANDS to `custode-save-file' file in PROJECT-ROOT.

Deletes the file if COMMANDS are empty."
  (let ((filename (concat (file-name-as-directory project-root) custode-save-file)))
    (if commands
        (with-temp-buffer
          (insert ";;; -*- lisp-data -*-\n")
          (let ((print-length nil)
                (print-level nil))
            (pp commands (current-buffer)))
          (write-region nil nil filename nil 'silent))
      (delete-file filename))))

(defun custode--read-project-commands (project-root)
  "Read project commands from `custode-save-file' file in PROJECT-ROOT."
  (let* ((filename (concat (file-name-as-directory project-root) custode-save-file))
         (read-data (when (file-exists-p filename)
                      (with-temp-buffer
                        (insert-file-contents filename)
                        (read (current-buffer)))))
         (commands))
    (dolist (item read-data)
      (when (stringp (car item))
        (let ((read-command (car item))
              (read-command-options (cdr item))
              (new-command '()))
          (when (and (assoc :task read-command-options)
                     (stringp (cdr (assoc :task read-command-options))))
            ;; Old format, use :task instead.
            (setq read-command (cdr (assoc :task read-command-options))))
          ;; Check for optional values.
          (when (and (assoc :positioning-function read-command-options)
                     (symbolp (cdr (assoc :positioning-function read-command-options)))
                     (memq (cdr (assoc :positioning-function read-command-options))
                           custode-buffer-positioning-functions))
            (push (cons :positioning-function (cdr (assoc :positioning-function read-command-options))) new-command))
          (push (cons read-command new-command) commands))))
    commands))

(provide 'custode)
;;; custode.el ends here
