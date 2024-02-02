;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "running tasks"
  (before-each
    ;; Silence the load message.
    (spy-on 'custode-load))
  (describe "custode-buffer-name"
    (it "should generate an adequate buffor name"
      (expect (custode-buffer-name "project" "task")
              :to-equal
              " *custode project task*")))

  (describe "custode--trigger"
    :var (custode--tasks custode--task-states)
    (before-each
      (setq custode--tasks '())
      (setq custode--task-states '())
      (spy-on 'custode--current-project-root :and-return-value "unrelated")
      (shut-up (custode-create-task "task" "unrelated command")
               (custode-enable-task "task"))
      (spy-on 'custode--current-project-root :and-return-value "project")
      (shut-up (custode-create-task "task" "the command")
               (custode-enable-task "task"))
      (spy-on 'custode--start))

    (it "triggers task running"
      (custode--trigger "project")
      (expect 'custode--start :to-have-been-called-with "project" "task" "the command" nil))

    (it "triggers multiple task running"
      (shut-up (custode-create-task "task2" "the other command")
               (custode-enable-task "task2"))
      (custode--trigger "project")
      (expect 'custode--start :to-have-been-called-with "project" "task" "the command" nil)
      (expect 'custode--start :to-have-been-called-with "project" "task2" "the other command" nil))

    (it "triggers task with args"
      (custode-set-task-args "task" "the arguments")
      (custode--trigger "project")
      (expect 'custode--start :to-have-been-called-with "project" "task" "the command the arguments" nil)
      ))

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
               1)))

    (describe "buffer display"
      (describe "per default, custode--position-buffer-beginning"
        (it "shows the beginning of the buffer"
          (assess-with-preserved-buffer-list
           (custode--start "project" "task" "false")
           (wait-for-custode-tasks)

           (with-current-buffer (get-buffer (custode-buffer-name "project" "task"))
             (expect (window-point (get-buffer-window (current-buffer)))
                     :to-equal
                     (point-min)))))

        (it "shows the beginning of the buffer, on reruns"
          (assess-with-preserved-buffer-list
           (custode--start "project" "task" "false")
           (wait-for-custode-tasks)
           (custode--start "project" "task" "false")
           (wait-for-custode-tasks)

           (with-current-buffer (get-buffer (custode-buffer-name "project" "task"))
             (expect (window-point (get-buffer-window (current-buffer)))
                     :to-equal
                     (point-min))))))

      (describe "custode--position-buffer-end"
        (it "shows the end of the buffer"
          (assess-with-preserved-buffer-list
           (custode--start "project" "task" "false" 'custode--position-buffer-end)
           (wait-for-custode-tasks)

           (let* ((buffer (get-buffer (custode-buffer-name "project" "task")))
                  (window (get-buffer-window buffer))
                  (expected-point (save-excursion
                                    (with-current-buffer buffer
                                      (goto-char (point-max))
                                      (vertical-motion (- (floor (/ (window-height window) 2))))
                                      (point)))))
             (with-current-buffer buffer
               (expect (window-point window)
                       :to-equal
                       expected-point)))))

        (it "shows the end of the buffer, on reruns"
            (assess-with-preserved-buffer-list
             (custode--start "project" "task" "false" 'custode--position-buffer-end)
             (wait-for-custode-tasks)
             (custode--start "project" "task" "false" 'custode--position-buffer-end)
             (wait-for-custode-tasks)

             (let* ((buffer (get-buffer (custode-buffer-name "project" "task")))
                    (window (get-buffer-window buffer))
                    (expected-point (save-excursion
                                      (with-current-buffer buffer
                                        (goto-char (point-max))
                                        (vertical-motion (- (floor (/ (window-height window) 2))))
                                        (point)))))
               (with-current-buffer buffer
                 (expect (window-point window)
                         :to-equal
                         expected-point)))))))))
