;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

(describe "config file"
  :var (custode--commands)

  (before-each
    (setq custode--commands '()))
  (describe "custode--write-project-commands"
    (it "writes the file"
      (assess-with-filesystem
       '()
       (setq tasks '(("task1" . ())
                     ("task2" . ((:positioning-function . custode--position-buffer-end)))))
       (custode--write-project-commands default-directory tasks)

       (expect (assess-file (concat (file-name-as-directory default-directory) custode-save-file))
               :to-equal
               ";;; -*- lisp-data -*-
((\"task1\")
 (\"task2\"
  (:positioning-function . custode--position-buffer-end)))
")))

    (it "deletes the file if no tasks"
      (assess-with-filesystem
       (list (list custode-save-file ";;; -*- lisp-data -*-
((\"task1\")
 (\"task2\"
  (:positioning-function . custode--position-buffer-end))
 (\"task3\"
  (:invalid . \"task three\")))
"))
       (setq tasks '())
       (custode--write-project-commands default-directory tasks)

       (expect (file-exists-p (concat (file-name-as-directory default-directory) custode-save-file))
               :to-equal
               nil)))

    (it "quietly handles if the file doesn't exist"
        (assess-with-filesystem
         '()
         (setq tasks '())
         (custode--write-project-commands default-directory tasks)

         (expect (file-exists-p (concat (file-name-as-directory default-directory) custode-save-file))
                 :to-equal
                 nil))))

  (describe "custode--read-project-commands"
    (it "reads the file"
      (assess-with-filesystem
       (list (list custode-save-file ";;; -*- lisp-data -*-
((\"task1\")
 (\"task2\"
  (:positioning-function . custode--position-buffer-end))
 (\"task3\"))
"))
       (expect (custode--read-project-commands default-directory)
               :to-have-same-items-as
               '(("task1" . ())
                 ("task2" . ((:positioning-function . custode--position-buffer-end)))
                 ("task3" . ())))))

    (it "reads file in old format"
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
       (expect (custode--read-project-commands default-directory)
               :to-have-same-items-as
               '(("task one" . ())
                 ("task two" . ((:positioning-function . custode--position-buffer-end)))
                 ;; This would have been ignored by the old
                 ;; version, but considering the number of users,
                 ;; it's not worth worring about.
                 ("task3" . ())))))))
