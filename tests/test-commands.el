;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "command management"
  :var (custode--commands)
  (before-each
    (setq custode--commands '())
    ;; Silence the load message.
    (spy-on 'custode-load))

  (describe "custode--current-project-root"
    (describe "without a project"
      (before-each
        (spy-on 'project-current :and-return-value nil))
      (it "returns nil"
        (expect (custode--current-project-root)
                :to-equal
                nil)
        ))
    (describe "with a project"
      (before-each
        (spy-on 'project-current :and-return-value '(vc . "/some/path/")))
      (it "returns the path"
        (expect (custode--current-project-root)
                :to-equal
                "/some/path/"))))

  (describe "custode--get-project"
    (it "returns the requested project"
      (spy-on 'custode--current-project-root :and-return-value "project")
      (shut-up (custode-add-command "task1")
               (custode-add-command "task2"))

      (expect (custode--get-project "project")
              :to-have-same-items-as
              '("project" . (("task1" . ())
                             ("task2" . ())))))

    (it "creates project on unknown root"
        (expect (custode--get-project "banana")
                :to-have-same-items-as
                '("banana") . ())))

  (describe "custode-add-command"
    (it "should add commands to current project"
      (spy-on 'custode--current-project-root :and-return-value "the-project/")
      (shut-up (custode-add-command "the command"))
      (expect custode--commands
              :to-equal
              '(("the-project/" .
                 (("the command" . ())))))

      (shut-up (custode-add-command "another command"))
      (expect (cdr (assoc "the-project/" custode--commands))
              :to-have-same-items-as
              '(("the command" . ())
                ("another command" . ()))))

    (it "errors when no project is active"
      (spy-on 'custode--current-project-root :and-return-value nil)
      (expect (custode-add-command "the command")
              :to-throw 'error)))

  (describe "custode-edit-command"
    (before-each
      (spy-on 'custode--current-project-root :and-return-value "the-project/")
      (spy-on 'yes-or-no-p :and-return-value t)
      (shut-up (custode-add-command "the command")))

    (it "should replace command in current project"
      (shut-up (custode-edit-command "the command" "new command"))
      (expect (cdr (assoc "the-project/" custode--commands))
              :to-have-same-items-as
              '(("new command")))))

  (describe "custode-delete-command"
    (before-each
      (spy-on 'custode--current-project-root :and-return-value "the-project/")
      (spy-on 'yes-or-no-p :and-return-value t)
      (shut-up (custode-add-command "the command")))

    (it "should remove commands from current project"
      (shut-up (custode-delete-command "the command"))
      (expect (cdr (assoc "the-project/" custode--commands))
              :to-have-same-items-as
              '())))

  (describe "custode-set-buffer-positioning"
    (before-each
      (spy-on 'custode--current-project-root :and-return-value "the-project/")
      (shut-up (custode-add-command "the command")))

    (it "sets the buffer positioning"
      (custode-set-buffer-positioning "the command" 'custode--position-buffer-end)
      (expect (cdr (assoc "the command" (cdr (assoc "the-project/" custode--commands))))
              :to-have-same-items-as
              '((:positioning-function . custode--position-buffer-end))))

    ;; Can only happen when called from Lisp.
    (it "errors on invalid positioning"
      (expect (custode-set-buffer-positioning "the command" 'banana)
              :to-throw 'error)))
  )
