;;; pcomplete-me-test.el --- Tests                   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Russell Sim

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

(ert-deftest pcmpl-me--flag-post-test ()
  (should
   (equal
    (pcmpl-me--arg-list '("--verbose" "--verbose=" :foobar "barfoo" :no-args))
    '((:no-args)
      (:foobar "barfoo")
      (:args "--verbose=" "--verbose")))))

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
                                 (pcomplete-arg 1))))))))))

(ert-deftest pcmpl-me--flag-inline-test ()
  (should
   (equal
    (let ((pcmpl-me-completers (append pcmpl-me-completers '(:test (lambda () '("test"))))))
      (pcmpl-me--flag-inline-matchers '((("--verbose" "-v"))
                                        (("--profile" "--profile=") . (:files))
                                        (("--output" "--output=") . (:list "json" "yaml"))
                                        (("--filter" "--filter=") . (:test)))))
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
    (pcmpl-me--flag-inline-matchers '((("--profile=") . (:files))
                                      (("--output=") . (:list "json" "yaml"))))
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

(provide 'pcomplete-me-test)
;;; pcomplete-me-test.el ends here
