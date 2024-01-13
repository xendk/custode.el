;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "config file"
  :var (custode--tasks)

  (before-each
    (setq custode--tasks '()))
  (describe "custode--write-project-tasks"
    (it "writes the file"
      (assess-with-filesystem
          '()
        (setq tasks '(("task1" . ((:task . "task one")))
                      ("task2" . ((:task . "task two")))))
        (custode--write-project-tasks default-directory tasks)

        (expect (assess-file (concat (file-name-as-directory default-directory) custode-save-file))
                :to-equal
                ";;; -*- lisp-data -*-
((\"task1\"
  (:task . \"task one\"))
 (\"task2\"
  (:task . \"task two\")))
")
        )
      ))

  (describe "custode--read-project-tasks"
    (it "reads the file"
      (assess-with-filesystem
          (list (list custode-save-file ";;; -*- lisp-data -*-
((\"task1\"
  (:task . \"task one\"))
 (\"task2\"
  (:task . \"task two\")))
"))
        (expect (custode--read-project-tasks default-directory)
                :to-have-same-items-as
                '(("task1" . ((:task . "task one")))
                  ("task2" . ((:task . "task two")))))))))
