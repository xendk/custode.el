;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "state management"
  :var (custode--project-states custode--command-states)
  (before-each
    (setq custode--project-states '())
    (setq custode--command-states '())
    (setq custode-autosave nil))

  (describe "custode--(get|set)-project-state"
    (it "allows for setting and getting state"
      (custode--set-project-state "test" :some-key "value")
      (expect (custode--get-project-state "test" :some-key)
              :to-equal "value")
      (expect (custode--get-project-state "test" :someother-key)
              :to-equal nil)
      (expect (custode--get-project-state "test2" :some-key)
              :to-equal nil)
      (custode--set-project-state "test" :some-key nil)
      (expect (custode--get-project-state "test" :some-key)
              :to-equal nil)
      ;; Ensure we don't have lingering empty projects.
      (expect custode--project-states
              :to-have-same-items-as
              nil)))

  (describe "custode-edit-command"
    (it "should move state to the new command"
      (spy-on 'custode--current-project-root :and-return-value "test")
      (shut-up (custode-add-command "the command"))
      (let ((command-state (custode--current-command-state "test" "the command")))
        (push (cons :running 1) (cdr command-state))
        (expect custode--command-states
                :to-have-same-items-as
                '(("test\0the command" . ((:running . 1)))))
        (shut-up (custode-edit-command "the command" "new command"))
        (expect custode--command-states
                :to-have-same-items-as
                '(("test\0new command" . ((:running . 1))))))))

  (describe "custode--current-command-state"
    (it "creates new command states"
      (expect (custode--current-command-state "test" "task")
              :to-equal
              '("test\0task" . ()))
      (expect custode--command-states
              :to-equal
              '(("test\0task" . ()))))

    (it "allows for modifying command states"
      ;; Unrelated task.
      (custode--current-command-state "test" "task2")
      (let ((command-state (custode--current-command-state "test" "task")))
        (push (cons :enabled t) (cdr command-state))
        (expect (custode--current-command-state "test" "task")
                :to-equal
                '("test\0task" . ((:enabled . t))))
        (expect custode--command-states
                :to-have-same-items-as
                '(("test\0task" . ((:enabled . t)))
                  ("test\0task2" . ()))))))


  (describe "custode--set-command-state"
    (it "allows for setting command states"
      (custode--set-command-state "test" "task2" :something t)
      (custode--set-command-state "test" "task" :watching t)
      (expect custode--command-states
              :to-have-same-items-as
              '(("test\0task" . ((:watching . t)))
                ("test\0task2" . ((:something .  t))))))

    (it "unsets when given a nil value"
      (setq custode--command-states '(("test\0task" . ((:watching . t) (:other . t)))))
      (custode--set-command-state "test" "task" :watching nil)
      (expect custode--command-states
              :to-have-same-items-as
              '(("test\0task" . ((:other . t)))))

      (custode--set-command-state "test" "task" :other nil)
      (expect custode--command-states
              :to-have-same-items-as
              nil)))

  (describe "custode--get-command-state"
    (it "allows for getting command state"
      (setq custode--command-states '(("test\0task" . ((:watching . t)))
                                      ("test\0task2" . ((:something .  t)))))
      (expect (custode--get-command-state "test" "task" :watching)
              :to-equal t)
      (expect (custode--get-command-state "test" "task2" :something)
              :to-equal t)
      (expect (custode--get-command-state "test" "task" :something)
              :to-equal nil)))

  (describe "custode--(get|set)-command-state"
    (it "cleans up empty states"
      (custode--set-command-state "test" "task2" :something t)
      (custode--set-command-state "test" "task" :watching t)
      (custode--set-command-state "test" "task2" :something nil)
      (custode--set-command-state "test" "task" :watching nil)
      (custode--get-command-state "test" "task" :watching)
      (custode--get-command-state "test" "task2" :watching)
      (expect custode--command-states
              :to-have-same-items-as
              nil)))

  (describe "command :watching state"
    :var (custode--commands custode--command-states)

    (before-each
      ;; Without copy-tree, all tests would work on the same list.
      (setq custode--commands (copy-tree
                               '(("project" .
                                  (("task1" . ())
                                   ("task2" . ())))
                                 ("project2" .
                                  (("task1" . ()))))))
      (setq custode--command-states (copy-tree
                                     '(("project\0task1" . ((:watching t)))
                                       ("project\0task2" . ((:nothing t)))
                                       ("project2\0task1" . ((:watching t)))))))

    (describe "custode--command-watching-p"
      (it "returns t for watched commands"
        (expect (custode--command-watching-p "project" "task1")
                :to-be t))

      (it "returns nil for not watched commands"
        (expect (custode--command-watching-p "project" "task2")
                :to-be nil))

      (it "returns nil for unknown commands"
        (expect (custode--command-watching-p "project" "task3")
                :to-be nil)))

    (describe "custode--get-watching-commands"
      (it "returns watched commands"
        (expect (custode--get-watching-commands "project")
                :to-have-same-items-as
                '("task1"))
        (expect (custode--get-watching-commands "project2")
                :to-have-same-items-as
                '("task1"))))

    (describe "custode-watch"
      (it "watches commands"
        (spy-on 'custode--current-project-root :and-return-value "project")

        (shut-up (custode-watch "task2"))
        (expect (custode--get-watching-commands "project")
                :to-have-same-items-as
                '("task2" "task1")))

      (it "errors on unknown project"
        (spy-on 'custode--current-project-root :and-return-value nil)
        (expect (custode-watch "task1")
                :to-throw))

      ;; Interactively, this is not possible, but it is when calling
      ;; from lisp.
      (it "errors on unknown command"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (expect (custode-watch "taskx")
                :to-throw))

      (it "triggers command when called interactively"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (spy-on 'custode--trigger)

        (shut-up (funcall-interactively 'custode-watch "task2"))
        (expect 'custode--trigger
                :to-have-been-called-with
                "project" '("task2")))

      (it "doesn't trigger command when called interactively with prefix argument"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (spy-on 'custode--trigger)

        (let ((current-prefix-arg '(4)))
          (shut-up (funcall-interactively 'custode-watch "task2")))
        (expect 'custode--trigger
                :not :to-have-been-called
                )))

    (describe "custode-unwatch"
      (it "stops watching commands"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (shut-up (custode-unwatch "task1"))
        (expect (custode--get-watching-commands "project")
                :to-have-same-items-as
                nil))

      (it "errors on unknown project"
        (spy-on 'custode--current-project-root :and-return-value nil)
        (expect (custode-unwatch "task1")
                :to-throw))

      (it "errors on unknown command"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (expect (custode-unwatch "taskx")
                :to-throw))

      (it "deletes buffer when unwatching command"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (let ((buffer-name (custode-buffer-name "project" "task1")))
          (get-buffer-create buffer-name)
          (shut-up (funcall-interactively 'custode-unwatch "task1"))
          (expect (get-buffer buffer-name)
                  :to-be nil)))

      (it "leaves buffer alone with prefix argument"
        (spy-on 'custode--current-project-root :and-return-value "project")
        (let ((buffer-name (custode-buffer-name "project" "task1")))
          (get-buffer-create buffer-name)
          (let ((current-prefix-arg '(4)))
            (shut-up (funcall-interactively 'custode-unwatch "task1")))
          (expect (get-buffer buffer-name)
                  :not :to-be nil)))))

  (describe "command :args state"
    (before-each
      (spy-on 'custode--current-project-root :and-return-value "project"))

    (describe "custode-set-command-args"
      (it "sets args state"
        (with-simulated-input ("task" "RET" (insert "command args") "RET")
          (call-interactively #'custode-set-command-args))
        (expect custode--command-states
                :to-have-same-items-as
                '(("project\0task" . ((:args . "command args"))))))
      (it "unsets args state on empty arg"
        (with-simulated-input ("task" "RET" (insert "command args") "RET"
                               "task" "RET C-a C-k RET")
          (call-interactively #'custode-set-command-args)
          (call-interactively #'custode-set-command-args))
        (expect custode--command-states
                :to-have-same-items-as
                nil)))))
