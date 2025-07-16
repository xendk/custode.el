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

;; Emacs centered "file watcher". See README.md for description and
;; usage.

;; Todo:
;; - Change watch and unwatch to a custode-watch-toggle function.
;; - Rename `custode-save-file' to `.custode.eld'. With auto-migration.
;; - Add debounce so that opening magit with many unsaved files
;;   doesn't re-run the command many times in a row.
;; - Rewrite UI to use transient.

;;; Code:

(require 'compile)
(require 'project)

(defvar custode--commands '()
  "List of known commands across all projects.

The format is:
\((\"project_root\" . (
  \"eldev test\" (
    (:positioning-function . custode--position-buffer-end)))))")

(defvar custode--command-states '()
  "Stores runtime state of commands.

The format is:
\((\"project_root\\0command\" . (
    (:watching . t)
)))")

(defvar custode--project-states '()
  "Stores runtime state of projects.

The format is:
\((\"project_root\" . (
    (:running . 0)
)))")

(defvar custode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'custode-add-command)
    (define-key map "k" 'custode-delete-command)
    (define-key map "e" 'custode-edit-command)
    (define-key map "w" 'custode-watch)
    (define-key map "W" 'custode-unwatch)
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
      (let* ((running (custode--get-project-state :running (custode--current-project-root))))
        (if (and (numberp running) (> running 0))
            (propertize " 👁" 'face 'compilation-mode-line-run)
          " 👁"))))
  "Mode line lighter for Custode.

The value of this variable is a mode line template as in
`mode-line-format'.")

(put 'custode-lighter 'risky-local-variable t)

(defvar-local custode-position-function
    "Function to call to position output buffer.

Automatically set in relevant buffers by custode--start."
  nil)

(defcustom custode-autosave
  t
  "Whether to automatically save tasks on changes."
  :group 'custode
  :type 'boolean)

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

(defmacro custode-with-current-project (&rest body)
  "Evaluate BODY with current project as PROJECT.

A project is a list where the car is the project root and the cdr is the
commands."
  (declare (indent 0) (debug t))
  `(let* ((project (custode--get-project (custode--current-project-root))))
     (progn ,@body)))

(defun custode-buffer-name (project-root command)
  "Get the buffer name for the COMMAND in PROJECT-ROOT project."
  (concat " *custode " project-root " " command "*"))

(defun custode-add-command (command)
  "Add COMMAND to the current project.

After adding a command, use `custode-watch' to continuously run
it."
  (interactive "sCommand: ")
  (custode-with-current-project
    (push (cons command '()) (cdr project))
    (custode-autosave)
    (message "Created \"%s\"" command)))

(defun custode-edit-command (command new-command)
  "Edit COMMAND to NEW-COMMAND.

Changes the command and carries state over."
  (interactive
   (let ((command (custode--completing-read-command "Edit command")))
     (list
      command
      (read-string "New command: "
                   command
                   'custode-command-history))))
  (custode-with-current-project
    (let* ((project-root (car project))
           (old-state-key (custode--commmand-state-key project-root command))
           (new-state-key (custode--commmand-state-key project-root new-command)))
      (setcar (assoc command (cdr project)) new-command)
      ;; Move state over.
      (when (assoc old-state-key custode--command-states)
        (setcar (assoc old-state-key custode--command-states) new-state-key))
      (when (and (custode--command-watching-p project-root new-command)
                 (called-interactively-p 'any)
                 (not current-prefix-arg))
        (kill-buffer (custode-buffer-name project-root command))
        (custode--trigger project-root (list new-command)))
      (custode-autosave)
      (message "Changed command"))))

(defun custode-delete-command (command)
  "Delete COMMAND from the current project."
  (interactive
   (list
    (custode--completing-read-command "Delete command")))
  (custode-with-current-project
    (if (yes-or-no-p (format "Really delete \"%s\"? " command))
        (progn
          (setcdr project (assoc-delete-all command (cdr project)))
          (custode-autosave)
          (message "Deleted \"%s\"" command))
      (message "Command not deleted"))))

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
    (when (and (called-interactively-p 'any) (not current-prefix-arg))
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
    (when (and (called-interactively-p 'any) (not current-prefix-arg))
      (kill-buffer (custode-buffer-name project-root command)))
    (message "Stopped watching \"%s\"" command)))

(defun custode-load ()
  "Load project commands from `custode-save-file' file in project root."
  (interactive)
  (custode-with-current-project
    (setcdr project (custode--read-project-commands (car project)))
    (message "Loaded project commands.")))

(defun custode-save ()
  "Write project commands to `custode-save-file' file in project root."
  (interactive)
  (let ((project-root (custode--current-project-root)))
    (custode--write-project-commands project-root (custode--get-current-project-commands))
    (message "Saved project commands")))

(defun custode-set-buffer-positioning (command positioning-function)
  "Set the positioning function for the Custode buffer for COMMAND.

POSITIONING-FUNCTION should be either custode--position-buffer-end or
custode--position-buffer-beginning."
  (interactive
   (list
    (custode--completing-read-command "Set buffer positioning for")
    (intern (completing-read "Positioning function: "
                             (mapcar 'symbol-name custode-buffer-positioning-functions)
                             nil t))))
  (unless (member positioning-function custode-buffer-positioning-functions)
    (error "Unknown positioning function %s" positioning-function))

  (custode-with-current-project
    (let* ((command (car (assoc command (cdr project)))))
      (unless command
        (error "Unknown command \"%s\"" command))
      (custode--set-command-option (car project) command :positioning-function positioning-function)
      (custode-autosave)
      (message "Set positioning function for \"%s\"" command))))

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
                   (custode--get-command-state (custode--current-project-root) command :args)
                   'custode-args-history))))
  (let* ((args (string-trim args))
         (project-root (custode--current-project-root)))
    (custode--set-command-state project-root command :args (if (equal args "") nil args))
    (when (and (called-interactively-p 'any)
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
  (when-let* ((running (custode--get-project-state :running (custode--current-project-root))))
    (custode--set-project-state :running (1- running) (custode--current-project-root)))
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
    (set-window-point window new-point)))

(defun custode--position-buffer-beginning (buffer)
  "Position BUFFER at the beginning."
  (set-window-point (get-buffer-window buffer t) (point-min)))

(defun custode--after-save-hook ()
  "After save hook for custode-mode.

Triggers running enabled commands if the file is in a project."
  (ignore-errors
    (custode--trigger (custode--current-project-root))))

(defun custode--current-project-root ()
  "Get the project root of the current project, or signals error if no project."
  (let ((current-project (project-current)))
    (unless current-project
      (user-error "Not in a project"))
    (project-root current-project)))

(defun custode--commmand-state-key (project-root command)
  "Get the state key of COMMAND in PROJECT-ROOT."
  (concat project-root "\0" command))

(defun custode--set-command-option (project-root command option value)
  "Set OPTION to VALUE for COMMAND in PROJECT-ROOT."
  (custode-with-current-project
    (let* ((command (assoc command (cdr project))))
      (unless command
        (error "Unknown command \"%s\"" command))
      (if (assoc option (cdr command))
          (setf (cdr (assoc option (cdr command))) value)
        (push (cons option value) (cdr command))))))

(defun custode--get-command-option (project-root command option)
  "Get OPTION for COMMAND in PROJECT-ROOT."
  (custode-with-current-project
    (let* ((command (assoc command (cdr project))))
      (unless command
        (error "Unknown command \"%s\"" command))
      (cdr (assoc option (cdr command))))))

(defun custode--current-command-state (project-root command)
  "Return COMMAND state for PROJECT-ROOT.

Creates the state if not found."
  (let ((key (custode--commmand-state-key project-root command)))
    (unless (assoc key custode--command-states)
      (let ((val (copy-tree '())))
        (push (cons key val) custode--command-states)))
    (assoc key custode--command-states)))

(defun custode--set-command-state (project-root command key value)
  "Set the KEY state for the COMMAND in PROJECT-ROOT to VALUE."
  (let ((state (custode--current-command-state project-root command)))
    (if value
        (if (assoc key (cdr state))
            (setf (cdr (assoc key (cdr state))) value)
          (push (cons key value) (cdr state)))
      ;; If nil value, delete key.
      (setf (cdr state) (assoc-delete-all key (cdr state)))
      ;; TODO a bit inefficient to delete the list that
      ;; `custode--current-command-state' might just have added.
      (when (= 1 (length state))
        (setq custode--command-states
              (assoc-delete-all (custode--commmand-state-key project-root command) custode--command-states))))))

(defun custode--get-command-state (project-root command key)
  "Get the KEY state of the COMMAND in PROJECT-ROOT."
  (let ((state (custode--current-command-state project-root command)))
    (cdr (assoc key (cdr state)))))

(defun custode--current-project-state (project-root)
  "Return project state for PROJECT-ROOT.

Creates the project state if not found."
  (unless (assoc project-root custode--project-states)
    (let ((val (copy-tree '())))
      (push (cons project-root val) custode--project-states)))
  (assoc project-root custode--project-states))

(defun custode--get-project-state (key project-root)
  "Get the KEY project state in PROJECT-ROOT."
  (cdr (assoc key (cdr (custode--current-project-state project-root)))))

(defun custode--set-project-state (key value project-root)
  "Set the KEY project state to VALUE in PROJECT-ROOT."
  (if value
      (if (custode--get-project-state key project-root)
          (setcdr (custode--get-project-state key project-root) value)
        (push (cons key value) (cdr (custode--current-project-state project-root))))
    (assoc-delete-all key (custode--current-project-state project-root))))

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
  (custode-with-current-project
    (cdr project)))

(defun custode--set-command-watching (project-root command state)
  "Set COMMAND watching state.

PROJECT-ROOT is the project root and STATE is t or nil."
  (custode-with-current-project
    (or (assoc command (cdr project))
        (error "Unknown command \"%s\"" command))
    (custode--set-command-state project-root command :watching state)))

(defun custode--command-watching-p (project-root command)
  "Check whether the COMMAND in PROJECT-ROOT is watching."
  (if (member command (custode--get-watching-commands project-root))
      t nil))

(defun custode--get-watching-commands (project-root)
  "Get watching commands for PROJECT-ROOT.

Returns a list of commands."
  (when-let* ((commands (alist-get project-root custode--commands nil nil 'equal)))
    (delq nil
          (mapcar
           (lambda (command)
             (when (custode--get-command-state project-root (car command) :watching)
               (car command)))
           commands))))

(defun custode--completing-read-command (prompt)
  "Use `completing-read' to read a command in the current project.

PROMPT is the prompt to use."
  (unless (custode--get-current-project-commands)
    (user-error "No commands defined in project"))
  (completing-read (concat prompt ": ") #'custode--command-completion-table nil t))

(defun custode--command-completion-table (str pred flag)
  "Completion table for custode commands.

Mostly for adding a affixation-function.

STR, PRED and FLAG is defined by the completion system."
  (pcase flag
    ('metadata '(metadata (category . 'custode-command)
                          (affixation-function . custode--command-completion-table-affixation)))
    (_ (all-completions str (custode--get-current-project-commands) pred))))

(defun custode--command-completion-table-affixation (completions)
  "Affixation function to decorate COMPLETIONS.

Adds a checkmark to watched commands and the current arguments."
  (custode-with-current-project
    (mapcar (lambda (c)
              (let* ((command (car (assoc c (cdr project))))
                     (args (custode--get-command-state (car project) c :args)))
                (list c
                      (if (custode--command-watching-p (car project) command)
                          "✓ " "  ")
                      (when args
                        (propertize (concat " " args) 'face 'custode-completions-args)))))
            completions)))

(defun custode--trigger (project-root &optional commands)
  "Trigger commands on PROJECT-ROOT.

Optionally supply COMMANDS to trigger these specific commands,
regardless of whether they're enabled or not."
  (let ((commands (or commands (custode--get-watching-commands project-root)))
        (default-directory project-root))
    (dolist (command commands)
      (custode--start project-root
                      command
                      (custode--get-command-state project-root command :args)
                      (custode--get-command-option project-root command :positioning-function)))))

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
                                        'custode-command-mode)))
        (if-let* ((running (custode--get-project-state :running (custode--current-project-root))))
            (custode--set-project-state :running (1+ running) (custode--current-project-root)))
        (with-current-buffer buffer
          (setq custode-position-function
                (or position-function 'custode--position-buffer-beginning))))
      (force-mode-line-update t))))

(defun custode--write-project-commands (project-root commands)
  "Write project COMMANDS to `custode-save-file' file in PROJECT-ROOT.

Deletes the file if COMMANDS are empty."
  (let ((filename (concat (file-name-as-directory project-root) custode-save-file))
        (commands (sort commands (lambda (a b) (string< (car a) (car b))))))
    (if commands
        (with-temp-buffer
          ;; Sort options.
          (dolist (command commands)
            (setf (cdr command) (sort (cdr command) (lambda (a b) (string< (car a) (car b))))))
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

(defun custode-autosave ()
  "Save commands if autosave is turned on."
  (when custode-autosave
    (custode-save)))

(provide 'custode)
;;; custode.el ends here
