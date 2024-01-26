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
                      ("task2" . ((:task . "task two")
                                  (:positioning-function . custode--position-buffer-end)))))
        (custode--write-project-tasks default-directory tasks)

        (expect (assess-file (concat (file-name-as-directory default-directory) custode-save-file))
                :to-equal
                ";;; -*- lisp-data -*-
((\"task1\"
  (:task . \"task one\"))
 (\"task2\"
  (:task . \"task two\")
  (:positioning-function . custode--position-buffer-end)))
")))

    (it "deletes the file if no tasks"
      (assess-with-filesystem
       (list (list custode-save-file ";;; -*- lisp-data -*-
((\"task1\"
  (:task . \"task one\"))
 (\"task2\"
  (:task . \"task two\")
  (:positioning-function . custode--position-buffer-end))
 (\"task3\"
  (:invalid . \"task three\")))
"))
       (setq tasks '())
       (custode--write-project-tasks default-directory tasks)

       (expect (file-exists-p (concat (file-name-as-directory default-directory) custode-save-file))
               :to-equal
               nil)))
    (it "quietly handles if the file doesn't exist"
      (assess-with-filesystem
       '()
       (setq tasks '())
       (custode--write-project-tasks default-directory tasks)

       (expect (file-exists-p (concat (file-name-as-directory default-directory) custode-save-file))
               :to-equal
               nil))))

  (describe "custode--read-project-tasks"
    (it "reads the file"
      (assess-with-filesystem
          (list (list custode-save-file ";;; -*- lisp-data -*-
((\"task1\"
  (:task . \"task one\"))
 (\"task2\"
  (:task . \"task two\")
  (:positioning-function . custode--position-buffer-end))
 (\"task3\"
  (:invalid . \"task three\")))
"))
          (expect (custode--read-project-tasks default-directory)
                  :to-have-same-items-as
                  '(("task1" . ((:task . "task one")))
                    ("task2" . ((:positioning-function . custode--position-buffer-end)
                                (:task . "task two")))))))))
