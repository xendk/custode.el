;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "running tasks"
  (describe "custode--start"
    (it "runs task in background"
      (assess-with-preserved-buffer-list
       (custode--start "project" "task" "true")
       (wait-for-custode-tasks)

       ;; There should be a buffer for the task.
       (expect (get-buffer (concat " *custode-task " default-directory " task*"))
               :not :to-be nil)
       ;; But still only one window.
       (expect (length (window-list))
               :to-equal
               1)))

    (it "pops up on error"
      (assess-with-preserved-buffer-list
       (custode--start "project" "task" "false")
       (wait-for-custode-tasks)

       (expect (get-buffer (concat " *custode-task " default-directory " task*"))
               :not :to-be nil)

       ;; It should have popped up a window.
       (expect (length (window-list))
               :to-equal
               2)))

    (it "removes window again on success"
      (assess-with-preserved-buffer-list
       (custode--start "project" "task" "false")
       (wait-for-custode-tasks)

       (expect (get-buffer (concat " *custode-task " default-directory " task*"))
               :not :to-be nil)

       ;; It should have popped up a window.
       (expect (length (window-list))
               :to-equal
               2)

       (custode--start "project" "task" "true")
       (wait-for-custode-tasks)
       (expect (length (window-list))
               :to-equal
               1))))

  (describe "custode--trigger"
    :var (custode--tasks custode--task-states)
    (before-each
      (setq custode--tasks '())
      (setq custode--task-states '())
      (spy-on 'custode--current-project-root :and-return-value "unrelated")
      (custode-add-task "task" "unrelated command")
      (custode-enable-task "task")
      (spy-on 'custode--current-project-root :and-return-value "project")
      (custode-add-task "task" "the command")
      (custode-enable-task "task")
      (spy-on 'custode--start))

    (it "triggers task running"
      (custode--trigger "project")
      (expect 'custode--start :to-have-been-called-with "project" "task" "the command"))

    (it "triggers multiple task running"
      (custode-add-task "task2" "the other command")
      (custode-enable-task "task2")
      (custode--trigger "project")
      (expect 'custode--start :to-have-been-called-with "project" "task" "the command")
      (expect 'custode--start :to-have-been-called-with "project" "task2" "the other command"))

    (it "triggers task with args"
      (custode-set-task-args "task" "the arguments")
      (custode--trigger "project")
      (expect 'custode--start :to-have-been-called-with "project" "task" "the command the arguments")
      )))

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

    (it "return nil on unknown project"
      (expect (custode--get-project "banana")
              :to-be
              nil)))

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
              :to-throw 'error))))

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
