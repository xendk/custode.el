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
       (expect (get-buffer (custode-buffer-name "project" "task"))
               :not :to-be nil)
       ;; But still only one window.
       (expect (length (window-list))
               :to-equal
               1)))

    (it "pops up on error"
      (assess-with-preserved-buffer-list
       (custode--start "project" "task" "false")
       (wait-for-custode-tasks)

       (expect (get-buffer (custode-buffer-name "project" "task"))
               :not :to-be nil)

       ;; It should have popped up a window.
       (expect (length (window-list))
               :to-equal
               2)))

    (it "removes window again on success"
      (assess-with-preserved-buffer-list
       (custode--start "project" "task" "false")
       (wait-for-custode-tasks)

       (expect (get-buffer (custode-buffer-name "project" "task"))
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
      ))

  (describe "custode-buffer-name"
    (it "should generate an adequate buffor name"
      (expect (custode-buffer-name "project" "task")
              :to-equal
              " *custode project task*"))))
