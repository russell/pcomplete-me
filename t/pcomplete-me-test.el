;;; pcomplete-me-test.el --- Tests                   -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Russell Sim

;; Author: Russell Sim <russell.sim@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'pcomplete-me)

(cl-defmacro pcmpl-me-test (command (test-fn &key inherit-global-flags))
  (declare (indent 2))
  (cl-flet ((test-name (&rest args) (intern (mapconcat 'symbol-name args "-"))))
   (let* ((command-list (if (listp command) command (cons command nil)))
          (global-command (car command-list))
          (global-flags (intern (mapconcat 'symbol-name `(pcmpl ,global-command -global-flags) "-")))
          (subcommand-flags (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -flags) "-")))
          (subcommand-subcommands (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -subcommands) "-"))))
     `(progn
        (thunk-let ((actual-flags (funcall #',test-fn (quote ,command-list) :flags ,global-flags))
                    (actual-subcommands (funcall #',test-fn (quote ,command-list) :subcommand)))

          ;; Test for cases where we are missing flags in the Emacs side of the completion
          (ert-deftest ,(test-name subcommand-flags  '-are-missing) ()
            ,(format "Flags missing from Emacs completion %S" subcommand-flags)
            (should
             (equal
              (cl-set-difference actual-flags ,subcommand-flags :test #'string-equal)
              nil)))
          ;; Test for cases where we have extra flags in the Emacs side of the completion
          (ert-deftest ,(test-name subcommand-flags  '-has-unwanted-flags) ()
            ,(format "Extra flags found in Emacs completion command %S" subcommand-flags)
            (should
             (equal
              (cl-set-difference ,subcommand-flags actual-flags :test #'string-equal)
              nil)))

          ;; Test for cases where there are missing subcommands
          (ert-deftest ,(test-name subcommand-subcommands' -are-missing) ()
            ,(format "Subcommands missing from Emacs completion %S" subcommand-subcommands)
            (should
             (equal
              (cl-set-difference actual-subcommands ,subcommand-subcommands :test #'string-equal)
              nil)))
          ;; Test for cases where we have declared extra subcommands
          (ert-deftest ,(test-name subcommand-subcommands' -has-unwanted-subcommand) ()
            ,(format "Extra subcommands in the system version of command %S" subcommand-subcommands)
            (should
             (equal
              (cl-set-difference ,subcommand-subcommands actual-subcommands :test #'string-equal)
              nil))))))))

(ert-deftest pcmpl-me--flag-post-test ()
  (should
   (equal
    (pcmpl-me--arg-list
     '("--verbose" "--verbose=" :foobar "barfoo" :no-args))
    '((:no-args)
      (:foobar "barfoo")
      (:args "--verbose=" "--verbose")))))

(ert-deftest pcmpl-me--matcher-expression-test ()
  (should
   (equal
    (pcmpl-me--matcher-expression
     '((:files) (:args "--verbose=" "--verbose")))
    '(pcomplete-entries)))
  (should
   (equal
    (pcmpl-me--matcher-expression
     '((:list "json" "yaml") (:args "--output=" "--output")))
    '(pcmpl-me--complete-from-list "json" "yaml")))
  (should
   (equal
    (pcmpl-me--matcher-expression '((:args "--output=" "--output")))
    nil)))

(ert-deftest pcmpl-me--flag-post-test ()
  (should
   (equal
    (let ((pcmpl-me-completers (append pcmpl-me-completers '(:test (lambda () '("test"))))))
      (pcmpl-me--flag-post-matchers '(("--verbose" "-v")
                                      ("--profile" "--profile=" :files)
                                      ("--output" "--output=" :list "json" "yaml")
                                      ("--filter" "--filter=" :test))))
    '((when
          (pcomplete-match "\\`-" 1)
        (cond
         ((pcomplete-match "\\`--profile\\'" 1)
          (pcomplete-here*
           (pcomplete-entries))
          (pcmpl-me--context-set :profile
                                 (pcomplete-arg 1)))
         ((pcomplete-match "\\`--output\\'" 1)
          (pcomplete-here*
           (pcmpl-me--complete-from-list "yaml" "json"))
          (pcmpl-me--context-set :output
                                 (pcomplete-arg 1)))
         ((pcomplete-match "\\`--filter\\'" 1)
          (pcomplete-here*
           ((lambda nil
              '("test"))))
          (pcmpl-me--context-set :filter
                                 (pcomplete-arg 1)))))))))

(ert-deftest pcmpl-me--context-symbol-test ()
  (should
   (equal
    (pcmpl-me--context-symbol '("--foobar=" "--bar=" "-b="))
    :foobar))
  (should
   (equal
    (pcmpl-me--context-symbol '("-b=" "--bar=" "--foobar=" ))
    :foobar))
  (should
   (equal
    (pcmpl-me--context-symbol '("--foobar"))
    :foobar))
  (should
   (equal
    (pcmpl-me--context-symbol '("--foobar="))
    :foobar)))

(ert-deftest pcmpl-me--flag-inline-test ()
  (should
   (equal
    (let ((pcmpl-me-completers (append pcmpl-me-completers '(:test (lambda () '("test"))))))
      (pcmpl-me--flag-inline-matchers '(("--verbose" "-v")
                                        ("--profile" "--profile=" :files)
                                        ("--output" "--output=" :list "json" "yaml")
                                        ("--filter" "--filter=" :test))))
    '((when
          (pcomplete-match "\\`-" 0)
        (cond
         ((pcomplete-match "\\`--profile=\\(.*\\)" 0)
          (pcomplete-here*
           (pcomplete-entries)
           (pcomplete-match-string 1 0)
           t)
          (pcomplete-match "\\`--profile=\\(.*\\)" 1)
          (pcmpl-me--context-set :profile
                                 (pcomplete-match-string 1)))
         ((pcomplete-match "\\`--output=\\(.*\\)" 0)
          (pcomplete-here*
           (pcmpl-me--complete-from-list "yaml" "json")
           (pcomplete-match-string 1 0)
           t)
          (pcomplete-match "\\`--output=\\(.*\\)" 1)
          (pcmpl-me--context-set :output
                                 (pcomplete-match-string 1)))
         ((pcomplete-match "\\`--filter=\\(.*\\)" 0)
          (pcomplete-here*
           ((lambda nil
              '("test")))
           (pcomplete-match-string 1 0)
           t)
          (pcomplete-match "\\`--filter=\\(.*\\)" 1)
          (pcmpl-me--context-set :filter
                                 (pcomplete-match-string 1)))))))))

(ert-deftest pcmpl-me--flag-matcher-only-inline-test ()
  (should
   (equal
    (pcmpl-me--flag-inline-matchers '(("--profile=" :files)
                                      ("--output=" :list "json" "yaml")))
    '((when
          (pcomplete-match "\\`-" 0)
        (cond
         ((pcomplete-match "\\`--profile=\\(.*\\)" 0)
          (pcomplete-here*
           (pcomplete-entries)
           (pcomplete-match-string 1 0)
           t)
          (pcomplete-match "\\`--profile=\\(.*\\)" 1)
          (pcmpl-me--context-set :profile
                                 (pcomplete-match-string 1)))
         ((pcomplete-match "\\`--output=\\(.*\\)" 0)
          (pcomplete-here*
           (pcmpl-me--complete-from-list "yaml" "json")
           (pcomplete-match-string 1 0)
           t)
          (pcomplete-match "\\`--output=\\(.*\\)" 1)
          (pcmpl-me--context-set :output
                                 (pcomplete-match-string 1)))))))))

(ert-deftest pcmpl-me--flags ()
  "Should return a sorted set of flags"
  (should
   (equal
    (pcmpl-me--flags '(("--api-group" "--api-group=")
                       ("--cached")
                       ("--no-headers")
                       ("--namespaced")
                       ("--output" "--output=" "-o" :list kubectl-output-name-or-wide)
                       ("--sort-by" "--sort-by=" :list "name" "kind")
                       ("--verbs" "--verbs=" :null)))
    '("--api-group" "--api-group=" "--cached" "--namespaced" "--no-headers"
      "--output" "--output=" "--sort-by" "--sort-by=" "--verbs" "--verbs=" "-o"))))

(ert-deftest pcmpl-me--cache-expired ()
  "Verify if the time has expired"
  (should
   ;; Should not be expired
   (eql (pcmpl-me--cache-expired-p (time-subtract (current-time) -10)) nil))
  (should
   ;; Should be expired
   (eql (pcmpl-me--cache-expired-p (time-subtract (current-time) 10)) t)))

(ert-deftest pcmpl-me--cache-expire-oldest ()
  "Verify that expiring element from the hash works when it's full."
  (let ((cache (make-hash-table :test #'equal :size 3)))
    (puthash 'foo `(,(time-add (current-time) 10) foo) cache)
    (puthash 'bar `(,(time-add (current-time) 20) bar) cache)

    ;; Shouldn't expire because the hash isn't full
    (pcmpl-me--cache-expire-oldest cache)
    (should (equal (hash-table-keys cache) '(bar foo)))

    ;; Should expire because now the hash is full
    (puthash 'baz `(,(time-add (current-time) 15) baz) cache)
    (pcmpl-me--cache-expire-oldest cache)
    (should (equal (hash-table-keys cache) '(baz bar)))))

(pcmpl-me-global-args pcomplete-me-test
  :flags
  '(("--global-foo")
    ("--fizzbuzz" "--fizzbuzz=" :null)
    ("--global-bar")))

(pcmpl-me-command pcomplete-me-test
  :inherit-global-flags t
  :flags
  '(("--local-flag"))
  :subcommands
  '("command1" "command2"))

(pcmpl-me-command (pcomplete-me-test command2)
  :inherit-global-flags t
  :flags
  '(("--subcommand-flag")
    ("--foobar" "--foobar=" :list "baz" "baaz"))
  :subcommands
  '("subcommand1" "subcommand2"))

(defun pcomplete/pcomplete-me-test ()
  "Completion for kubectl."
  (let ((pcmpl-me--context nil))
    (unwind-protect
        (pcmpl-pcomplete-me-test)
      (when pcmpl-me-debug
        (message "pcomplete/pcomplete-me-test: pcmpl-me--context %S" pcmpl-me--context)))))

(ert-deftest pcmpl-me-test-global-flags ()
  ""
  (should
   (equal
    (with-temp-buffer
      (insert "pcomplete-me-test --global")
      (pcomplete)
      (buffer-string))
    "pcomplete-me-test --global-bar")))

(ert-deftest pcmpl-me-test-local-flags ()
  ""
  (should
   (equal
    (with-temp-buffer
      (insert "pcomplete-me-test --local")
      (pcomplete)
      (buffer-string))
    "pcomplete-me-test --local-flag ")))

(ert-deftest pcmpl-me-test-subcommands ()
  ""
  (should
   (equal
    (with-temp-buffer
      (insert "pcomplete-me-test co")
      (pcomplete)
      (buffer-string))
    "pcomplete-me-test command1")))

(ert-deftest pcmpl-me-test-nested-subcommands ()
  ""
  (should
   (equal
    (with-temp-buffer
      (insert "pcomplete-me-test command2 sub")
      (pcomplete)
      (buffer-string))
    "pcomplete-me-test command2 subcommand1")))

(ert-deftest pcmpl-me-test-nested-subcommand-flags ()
  ""
  (should
   (equal
    (with-temp-buffer
      (insert "pcomplete-me-test command2 --subcommand")
      (pcomplete)
      (buffer-string))
    "pcomplete-me-test command2 --subcommand-flag ")))

(ert-deftest pcmpl-me-test-nested-subcommand-flags-with-list ()
  ""
  (should
   (equal
    (with-temp-buffer
      (insert "pcomplete-me-test command2 --foo")
      (pcomplete)
      (insert " ")
      (pcomplete)
      (buffer-string))
    "pcomplete-me-test command2 --foobar baaz")))

(provide 'pcomplete-me-test)
;;; pcomplete-me-test.el ends here
