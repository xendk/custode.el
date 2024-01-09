;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "custode--start"
  (it "runs task in background"
    (assess-with-preserved-buffer-list
     (custode--start "task" "true")
     (wait-for-custode-tasks)

     ;; There should be a buffer for the task.
     (expect (get-buffer (concat "*custode-task " default-directory " task*"))
             :not :to-be nil)
     ;; But still only one window.
     (expect (length (window-list))
             :to-equal
             1)))

  (it "pops up on error"
    (assess-with-preserved-buffer-list
     (custode--start "task" "false")
     (wait-for-custode-tasks)

     (expect (get-buffer (concat "*custode-task " default-directory " task*"))
             :not :to-be nil)

     ;; It should have popped up a window.
     (expect (length (window-list))
             :to-equal
             2)))

  (it "removes window again on success"
    (assess-with-preserved-buffer-list
     (custode--start "task" "false")
     (wait-for-custode-tasks)

     (expect (get-buffer (concat "*custode-task " default-directory " task*"))
             :not :to-be nil)

     ;; It should have popped up a window.
     (expect (length (window-list))
             :to-equal
             2)

     (custode--start "task" "true")
     (wait-for-custode-tasks)
     (expect (length (window-list))
             :to-equal
             1))))

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

(describe "state management"
  (describe "custode--get-task-state"
    (it "creates new states"
      (let ((custode--task-states '()))
        (expect (custode--get-task-state "test" "task")
                :to-equal
                '((:active . nil)))
        (expect custode--task-states
                :to-equal
                '(("test\0task" . ((:active . nil)))))))

    (it "allows for modifying the state"
      (let ((custode--task-states '()))
        ;; Unrelated task.
        (custode--get-task-state "test" "task2")
        (setq task (custode--get-task-state "test" "task"))
        (setf (alist-get :active task) t)
        (expect (custode--get-task-state "test" "task")
                :to-equal
                '((:active . t)))
        (expect custode--task-states
                :to-have-same-items-as
                '(("test\0task" . ((:active . t)))
                  ("test\0task2" . ((:active)))))))))

(describe "task management 1"
  (before-each
    (setq custode--tasks (list)))

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

;; TODO when we have an add-task, merge these into "task management
;; 1", and let them add their fixture in their own before-each.
(describe "task management 2"
  :var (simple-fixture simple-state-fixture)

  (before-each
    ;; Without copy-tree, all tests would work on the same list.
    (setq simple-fixture (copy-tree
                          '(("project" .
                             (("task1" . ((:task . "task one")))
                              ("task2" . ((:task . "task two")))))
                            ("project2" .
                             (("task1" . ((:task . "task three"))))))))
    (setq simple-state-fixture (copy-tree
                                '(("project\0task1" . ((:active t)))
                                  ("project2\0task1" . ((:active t)))))))

  (describe "custode--get-project"
    (it "returns the requested project"
      (let ((custode--tasks simple-fixture))
        (expect (custode--get-project "project")
                :to-equal
                '("project" . (("task1" . ((:task . "task one")))
                               ("task2" . ((:task . "task two"))))))))

    (it "return nil on unknown project"
      (let ((custode--tasks simple-fixture))
        (expect (custode--get-project "banana")
                :to-be
                nil))))

  (describe "custode--get-active-tasks"
    (it "returns active tasks"
      (let ((custode--tasks simple-fixture)
            (custode--task-states simple-state-fixture))
        (expect (custode--get-active-tasks "project")
                :to-have-same-items-as
                '(("task1" "task one")))
        )))

  (describe "custode-enable-task"
    (it "enables inactive tasks"
      (spy-on 'custode--current-project-root :and-return-value "project")

      (let ((custode--tasks simple-fixture)
            (custode--task-states simple-state-fixture))
        (custode-enable-task "task2")
        (expect (custode--get-active-tasks "project")
                :to-have-same-items-as
                ;; The reversed order is a quirk of
                '(("task2" "task two") ("task1" "task one")))))

    (it "errors on unknown project"
      (spy-on 'custode--current-project-root :and-return-value nil)
      (let ((custode--tasks simple-fixture)
            (custode--task-states simple-state-fixture))
        (expect (custode-enable-task "task1")
                :to-throw)))

    ;; Interactively, this is not possible, but it is when calling
    ;; from lisp.
    (it "errors on unknown task"
      (spy-on 'custode--current-project-root :and-return-value "project")
      (let ((custode--tasks simple-fixture)
            (custode--task-states simple-state-fixture))
        (expect (custode-enable-task "project" "taskx")
                :to-throw))))

  (describe "custode-disable-task"
    (it "disables active tasks"
      (spy-on 'custode--current-project-root :and-return-value "project")
      (let ((custode--tasks simple-fixture)
            (custode--task-states simple-state-fixture))
        (custode-disable-task "task1")
        (expect (custode--get-active-tasks "project")
                :to-have-same-items-as
                '())))

    (it "errors on unknown project"
      (spy-on 'custode--current-project-root :and-return-value nil)
      (let ((custode--tasks simple-fixture)
            (custode--task-states simple-state-fixture))
        (expect (custode-disable-task "task1")
                :to-throw)))

    (it "errors on unknown task"
      (spy-on 'custode--current-project-root :and-return-value "project")
      (let ((custode--tasks simple-fixture)
            (custode--task-states simple-state-fixture))
        (expect (custode-disable-task "taskx")
                :to-throw)))))
