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

(ert-deftest pcmpl-me--flag-matchers-test ()
  (should
   (equal
    (let ((pcmpl-me-completers (append pcmpl-me-completers '(:test (lambda () '("test"))))))
     (pcmpl-me--flag-matchers '((("--verbose" "-v"))
                                (("--profile" "--profile=") . (:files))
                                (("--output" "--output=") . (:list "json" "yaml"))
                                (("--filter" "--filter=") . (:test)))))
    '(progn
       (when
           (pcomplete-match "^-" 0)
         (cond
          ((pcomplete-match "\\`--profile=\\(.*\\)" 0)
           (pcomplete-here*
            (pcomplete-entries)
            (pcomplete-match-string 1 0)))
          ((pcomplete-match "\\`--output=\\(.*\\)" 0)
           (pcomplete-here*
            (list "yaml" "json")
            (pcomplete-match-string 1 0)))
          ((pcomplete-match "\\`--filter=\\(.*\\)" 0)
           (pcomplete-here*
            ((lambda nil
               '("test")))
            (pcomplete-match-string 1 0)))))
       (when
           (pcomplete-match "^-" 1)
         (cond
          ((pcomplete-match "\\`--profile\\'" 1)
           (pcomplete-here*
            (pcomplete-entries)))
          ((pcomplete-match "\\`--output\\'" 1)
           (pcomplete-here*
            (list "yaml" "json")))
          ((pcomplete-match "\\`--filter\\'" 1)
           (pcomplete-here*
            ((lambda nil
               '("test")))))))))))

(provide 'pcomplete-me-test)
;;; pcomplete-me-test.el ends here
