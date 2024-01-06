(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "custode-start"
  (it "runs task in background"
    (assess-with-preserved-buffer-list
     (custode-start "true")
     (wait-for-custode-tasks)

     ;; There should be a buffer for the task.
     (expect (get-buffer "*custode-task test*")
             :not :to-be nil)
     ;; But still only one window.
     (expect (length (window-list))
             :to-equal
             1)))

  (it "pops up on error"
    (assess-with-preserved-buffer-list
     (custode-start "false")
     (wait-for-custode-tasks)

     (expect (get-buffer "*custode-task test*")
             :not :to-be nil)

     ;; It should have popped up a window.
     (expect (length (window-list))
             :to-equal
             2))))
