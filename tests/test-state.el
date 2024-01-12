;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "state management"
  :var (custode--project-states custode--task-states)
  (before-each
    (setq custode--project-states '())
    (setq custode--task-states '())
    )

  (describe "custode--get-project-state"
    (it "creates new project states"
      (expect (custode--get-project-state "test")
              :to-equal
              '("test" . ()))
      (expect custode--project-states
              :to-equal
              '(("test" . ()))))

    (it "allows for modifying project states"
      ;; Unrelated project.
      (custode--get-project-state "test2")
      (setq project (custode--get-project-state "test"))
      (push (cons :running 1) (cdr project))
      (expect (custode--get-project-state "test")
              :to-equal
              '("test" . ((:running . 1))))
      (expect custode--project-states
              :to-have-same-items-as
              '(("test" . ((:running . 1)))
                ("test2" . ())))))

  (describe "custode--get-task-state"
    (it "creates new task states"
      (expect (custode--get-task-state "test" "task")
              :to-equal
              '("test\0task" . ()))
      (expect custode--task-states
              :to-equal
              '(("test\0task" . ()))))

    (it "allows for modifying task states"
      ;; Unrelated task.
      (custode--get-task-state "test" "task2")
      (setq task (custode--get-task-state "test" "task"))
      (push (cons :active t) (cdr task))
      (expect (custode--get-task-state "test" "task")
              :to-equal
              '("test\0task" . ((:active . t))))
      (expect custode--task-states
              :to-have-same-items-as
              '(("test\0task" . ((:active . t)))
                ("test\0task2" . ())))))

  (describe "task :active state"
    :var (custode--tasks custode--task-states)

    (before-each
      ;; Without copy-tree, all tests would work on the same list.
      (setq custode--tasks (copy-tree
                            '(("project" .
                               (("task1" . ((:task . "task one")))
                                ("task2" . ((:task . "task two")))))
                              ("project2" .
                               (("task1" . ((:task . "task three"))))))))
      (setq custode--task-states (copy-tree
                                  '(("project\0task1" . ((:active t)))
                                    ("project2\0task1" . ((:active t)))))))

    (describe "custode--get-active-tasks"
      (it "returns active tasks"
        (expect (custode--get-active-tasks "project")
                :to-have-same-items-as
                '("task1"))))

    (describe "custode-enable-task"
      (it "enables inactive tasks"
        (spy-on 'custode--current-project-root :and-return-value "project")

        (custode-enable-task "task2")
        (expect (custode--get-active-tasks "project")
                :to-have-same-items-as
                '("task2" "task1")))

      (it "errors on unknown project"
        (spy-on 'custode--current-project-root :and-return-value nil)
        (expect (custode-enable-task "task1")
                :to-throw))

      ;; Interactively, this is not possible, but it is when calling
      ;; from lisp.
      (it "errors on unknown task"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (expect (custode-enable-task "project" "taskx")
                :to-throw)))

    (describe "custode-disable-task"
      (it "disables active tasks"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (custode-disable-task "task1")
        (expect (custode--get-active-tasks "project")
                :to-have-same-items-as
                '()))

      (it "errors on unknown project"
        (spy-on 'custode--current-project-root :and-return-value nil)
        (expect (custode-disable-task "task1")
                :to-throw))

      (it "errors on unknown task"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (expect (custode-disable-task "taskx")
                :to-throw))))

  (describe "task :args state"
    (before-each
      (spy-on 'custode--current-project-root :and-return-value "project"))

    (describe "custode-set-task-args"
      (it "sets args state"
        (custode-set-task-args "task" "command args")
        (expect custode--task-states
                :to-have-same-items-as
                '(("project\0task" . ((:args . "command args"))))))
      (it "unsets args state on empty arg"
        (custode-set-task-args "task" "command args")
        (custode-set-task-args "task" "")
        (expect custode--task-states
                :to-have-same-items-as
                '(("project\0task" . ())))))

    (describe "custode--get-task-args"
      (it "returns nil when no args is set"
        (expect (custode--get-task-args "project" "task")
                :to-equal
                nil))

      (it "returns the currently set args"
        (custode-set-task-args "task" "command args")
        (expect (custode--get-task-args "project" "task")
                :to-equal
                "command args")
        ))))
