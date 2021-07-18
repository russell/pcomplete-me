;;; pcmpl-kubectl-test.el --- pcmpl-kubectl Tests    -*- lexical-binding: t; -*-

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

(require 'pcmpl-kubectl)
(require 'bash-completion-export-utils)

(defconst pcmpl-kubectl-test-bashinit (format "%s/pcmpl-kubectl-test.sh"
                                              (file-name-directory load-file-name)))

(defun rs//bash-complete-kubectl-subcommand (command &optional global-flags)
  ""
  (rs//bash-complete-subcommand command '("KUBECONFIG=/dev/null")))


(cl-defmacro pcmpl-me-test (command (&key inherit-global-flags))
  (let* ((command-list (if (listp command) command (cons command nil)))
         (global-command (car command-list))
         (global-flags (intern (mapconcat 'symbol-name `(pcmpl ,global-command -global-flags) "-")))
         (subcommand-flags (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -flags) "-")))
         (subcommand-subcommands (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -subcommands) "-"))))
    `(progn
       (ert-deftest ,(intern (mapconcat 'symbol-name `(pcmpl ,@command-list -flags) "-")) ()
         (should
          (equal
           (cl-set-difference
            (cl-sort
             ,subcommand-flags
             'string-lessp)
            (let ((bash-completion-start-files `(,pcmpl-kubectl-test-bashinit)))
              (rs//bash-complete-flags ,(mapconcat 'symbol-name command-list " ") ,global-flags))
            :test #'string-equal)
           nil)))
       (ert-deftest ,(intern (mapconcat 'symbol-name `(pcmpl ,@command-list -subcommands) "-")) ()
         (should
          (equal
           (cl-set-difference
            ,subcommand-subcommands
            (let ((bash-completion-start-files `(,pcmpl-kubectl-test-bashinit)))
             (rs//bash-complete-kubectl-subcommand ,(mapconcat 'symbol-name command-list " ")))
            :test #'string-equal)
           nil))))))

(pcmpl-me-test (kubectl annotate) (:inherit-global-flags t))
(pcmpl-me-test (kubectl api-versions) (:inherit-global-flags t))
(pcmpl-me-test (kubectl api-resources) (:inherit-global-flags t))
(pcmpl-me-test (kubectl apply) (:inherit-global-flags t))


(provide 'pcmpl-kubectl-test)
;;; pcmpl-kubectl-test.el ends here
