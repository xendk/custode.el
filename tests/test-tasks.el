;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "task management"
  :var (custode--tasks)
  (before-each
    (setq custode--tasks '()))

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
      (custode-add-task "task1" "task one")
      (custode-add-task "task2" "task two")

      (expect (custode--get-project "project")
              :to-have-same-items-as
              '("project" . (("task1" . ((:task . "task one")))
                             ("task2" . ((:task . "task two")))))))

    (it "creates project on unknown root"
      (expect (custode--get-project "banana")
              :to-have-same-items-as
              '("banana") . ())))

  (describe "custode-add-task"
    (it "should add tasks to current project"
      (spy-on 'custode--current-project-root :and-return-value "the-project/")
      (custode-add-task "the-task" "the command")
      (expect custode--tasks
              :to-equal
              '(("the-project/" .
                 (("the-task" . ((:task . "the command")))))))

      (custode-add-task "another-task" "another command")
      (expect (cdr (assoc "the-project/" custode--tasks))
              :to-have-same-items-as
              '(("the-task" . ((:task . "the command")))
                ("another-task" . ((:task . "another command"))))))

    (it "errors when no project is active"
      (spy-on 'custode--current-project-root :and-return-value nil)
      (expect (custode-add-task "the-task" "the command")
              :to-throw 'error)))

  (describe "custode-set-buffer-positioning"
    (before-each
      (spy-on 'custode--current-project-root :and-return-value "the-project/")
      (custode-add-task "the-task" "the command"))

    (it "sets the buffer positioning"
      (custode-set-buffer-positioning "the-task" 'custode--position-buffer-end)
      (expect (cdr (assoc "the-task" (cdr (assoc "the-project/" custode--tasks))))
              :to-have-same-items-as
              '((:task . "the command")
                (:positioning-function . custode--position-buffer-end))))

    ;; Can only happen when called from Lisp.
    (it "errors on invalid positioning"
      (expect (custode-set-buffer-positioning "the-task" 'banana)
              :to-throw 'error)))
  )
