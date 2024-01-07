;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "custode-start"
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
             2))))

(describe "task management"
  :var (simple-fixture)

  (before-each
    ;; Without copy-tree, all tests would work on the same list.
    (setq simple-fixture (copy-tree
                          '(("project" .
                             (("task1" . ((:active . t)
                                          (:task . "task one")))
                              ("task2" . ((:active . nil)
                                          (:task . "task two")))))
                            ("project2" .
                             (("task1" . ((:active . t)
                                          (:task . "task three"))))))))
    )

  (describe "custode--get-tasks"
    (it "returns the requested project"
      (let ((custode--tasks simple-fixture))
        (expect (custode--get-tasks "project")
                :to-equal
                '(("task1" . ((:active . t)
                              (:task . "task one")))
                  ("task2" . ((:active . nil)
                              (:task . "task two")))))))

    (it "return nil on unknown project"
      (let ((custode--tasks simple-fixture))
        (expect (custode--get-tasks "banana")
                :to-be
                nil))))

  (describe "custode--get-active-tasks"
    (it "returns active tasks"
      (let ((custode--tasks simple-fixture))
        (expect (custode--get-active-tasks "project")
                :to-have-same-items-as
                '(("task1" "task one")))
        )))

  (describe "custode-enable-task"
    (it "enables inactive tasks"
      (let ((custode--tasks simple-fixture))
        (custode-enable-task "project" "task2")
        (expect (custode--get-active-tasks "project")
                :to-have-same-items-as
                ;; The reversed order is a quirk of
                '(("task2" "task two") ("task1" "task one")))))
    (it "errors on unknown project"
      (let ((custode--tasks simple-fixture))
        (expect (custode-enable-task "projectX" "task1")
                :to-throw)))
    (it "errors on unknown task"
      (let ((custode--tasks simple-fixture))
        (expect (custode-enable-task "project" "taskx")
                :to-throw))))

  (describe "custode-disable-task"
    (it "disables active tasks"
      (let ((custode--tasks simple-fixture))
        (custode-disable-task "project" "task1")
        (expect (custode--get-active-tasks "project")
                :to-have-same-items-as
                '())))
    (it "errors on unknown project"
      (let ((custode--tasks simple-fixture))
        (expect (custode-disable-task "projectX" "task1")
                :to-throw)))
    (it "errors on unknown task"
      (let ((custode--tasks simple-fixture))
        (expect (custode-disable-task "project" "taskx")
                :to-throw)))))
