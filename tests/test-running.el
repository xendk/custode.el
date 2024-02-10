;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "running commands"
  (before-each
    ;; Silence the load message.
    (spy-on 'custode-load))
  (describe "custode-buffer-name"
    (it "should generate an adequate buffor name"
      (expect (custode-buffer-name "project" "comamnd")
              :to-equal
              " *custode project comamnd*")))

  (describe "custode--trigger"
    :var (custode--commands custode--command-states)
    (before-each
      (setq custode--commands '())
      (setq custode--command-states '())
      (spy-on 'custode--current-project-root :and-return-value "unrelated")
      (shut-up (custode-add-command "unrelated command")
               (custode-watch "unrelated command"))
      (spy-on 'custode--current-project-root :and-return-value "project")
      (shut-up (custode-add-command "the command")
               (custode-watch "the command"))
      (spy-on 'custode--start))

    (it "runs watched commands"
      (custode--trigger "project")
      (expect 'custode--start :to-have-been-called-with "project" "the command" nil nil))

    (it "runs multiple watched commands"
      (shut-up (custode-add-command "the other command")
               (custode-watch "the other command"))
      (custode--trigger "project")
      (expect 'custode--start :to-have-been-called-with "project" "the command" nil nil)
      (expect 'custode--start :to-have-been-called-with "project" "the other command" nil nil))

    (it "runs commands with args"
      (custode-set-command-args "the command" "the arguments")
      (custode--trigger "project")
      (expect 'custode--start :to-have-been-called-with "project" "the command" "the arguments" nil)))

  (describe "custode--start"
    (it "runs command in background"
      (assess-with-preserved-buffer-list
       (custode--start "project" "true")
       (wait-for-custode-commands)

       ;; There should be a buffer for the command.
       (expect (get-buffer (custode-buffer-name "project" "true"))
               :not :to-be nil)
       ;; But still only one window.
       (expect (length (window-list))
               :to-equal
               1)))

    (it "pops up on error"
      (assess-with-preserved-buffer-list
       (custode--start "project" "false")
       (wait-for-custode-commands)

       (expect (get-buffer (custode-buffer-name "project" "false"))
               :not :to-be nil)

       ;; It should have popped up a window.
       (expect (length (window-list))
               :to-equal
               2)))

    (it "removes window again on success"
      (assess-with-preserved-buffer-list
       (custode--start "project" "false")
       (wait-for-custode-commands)

       (expect (get-buffer (custode-buffer-name "project" "false"))
               :not :to-be nil)

       ;; It should have popped up a window.
       (expect (length (window-list))
               :to-equal
               2)

       (custode--start "project" "false" "|| true")
       (wait-for-custode-commands)
       (expect (length (window-list))
               :to-equal
               1)))

    (describe "buffer display"
      (describe "per default, custode--position-buffer-beginning"
        (it "shows the beginning of the buffer"
          (assess-with-preserved-buffer-list
           (custode--start "project" "false")
           (wait-for-custode-commands)

           (with-current-buffer (get-buffer (custode-buffer-name "project" "false"))
             (expect (window-point (get-buffer-window (current-buffer)))
                     :to-equal
                     (point-min)))))

        (it "shows the beginning of the buffer, on reruns"
          (assess-with-preserved-buffer-list
           (custode--start "project" "false")
           (wait-for-custode-commands)
           (custode--start "project" "false")
           (wait-for-custode-commands)

           (with-current-buffer (get-buffer (custode-buffer-name "project" "false"))
             (expect (window-point (get-buffer-window (current-buffer)))
                     :to-equal
                     (point-min))))))

      (describe "custode--position-buffer-end"
        (it "shows the end of the buffer"
          (assess-with-preserved-buffer-list
           (custode--start "project" "false" nil 'custode--position-buffer-end)
           (wait-for-custode-commands)

           (let* ((buffer (get-buffer (custode-buffer-name "project" "false")))
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
             (custode--start "project" "false" nil 'custode--position-buffer-end)
             (wait-for-custode-commands)
             (custode--start "project" "false" nil 'custode--position-buffer-end)
             (wait-for-custode-commands)

             (let* ((buffer (get-buffer (custode-buffer-name "project" "false")))
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
