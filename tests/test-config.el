;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'custode)

;; (buttercup-define-matcher :to-look-like (a b)
;;   (let* ((spec (format-spec-make
;;                 ?a (format "%S" a)
;;                 ?b (format "%S" b))))

;;     (buttercup--test-expectation

;;         (equal
;;          (replace-regexp-in-string "[\n ]+" " " a)
;;          (replace-regexp-in-string "[\n ]+" " " b)
;;          )
;;       :expect-match-phrase
;;       "Expected `%a' to look like `%b', but it didn't."
;;       :expect-mismatch-phrase
;;       "Expected `%a' not to look like `%b', but it did.")))

(defun whitespace-normalize (string)
  "Normalize whitespace in STRING.

Different versions of Emacs renders the Lisp structures a bit differently."
  (replace-regexp-in-string "[\n ]+" " " string))

(describe "config file"
  :var (custode--commands)

  (before-each
    (setq custode--commands '()))
  (describe "custode--write-project-commands"
    (it "writes the file"
      (assess-with-filesystem
       '()
       (setq commands '(("task1" . ())
                        ("task2" . ((:positioning-function . custode--position-buffer-end)))))
       (custode--write-project-commands default-directory commands)

       (expect (whitespace-normalize (assess-file (concat (file-name-as-directory default-directory) custode-save-file)))
               :to-equal
               (whitespace-normalize ";;; -*- lisp-data -*-
((\"task1\")
 (\"task2\"
  (:positioning-function . custode--position-buffer-end)))
"))))

    (it "writes the file in consistent order"
      (assess-with-filesystem
       '()
       (setq commands '(("task4" . ())
                        ("task1" . ())
                        ("task3" . ())
                        ("task2" . ())))
       (custode--write-project-commands default-directory commands)

       (expect (whitespace-normalize (assess-file (concat (file-name-as-directory default-directory) custode-save-file)))
               :to-equal
               (whitespace-normalize ";;; -*- lisp-data -*-
((\"task1\")
 (\"task2\")
 (\"task3\")
 (\"task4\"))
"))

       (setq commands '(("task" . ((:omega . "omega")
                                   (:beta . "beta")
                                   (:alpha . "alpha")
                                   (:gamma . "gamma")))))
       (custode--write-project-commands default-directory commands)

       (expect (whitespace-normalize (assess-file (concat (file-name-as-directory default-directory) custode-save-file)))
               :to-equal
               (whitespace-normalize ";;; -*- lisp-data -*-
((\"task\"
  (:alpha . \"alpha\")
  (:beta . \"beta\")
  (:gamma . \"gamma\")
  (:omega . \"omega\")))
"))))

    (it "deletes the file if no commands"
      (assess-with-filesystem
       `((,custode-save-file ";;; -*- lisp-data -*-
((\"task1\")
 (\"task2\"
  (:positioning-function . custode--position-buffer-end))
 (\"task3\"
  (:invalid . \"task three\")))
"))
       (setq commands '())
       (custode--write-project-commands default-directory commands)

       (expect (file-exists-p (concat (file-name-as-directory default-directory) custode-save-file))
               :to-equal
               nil)))

    (it "quietly handles if the file doesn't exist"
        (assess-with-filesystem
         '()
         (setq commands '())
         (custode--write-project-commands default-directory commands)

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
       `((,custode-save-file ";;; -*- lisp-data -*-
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
