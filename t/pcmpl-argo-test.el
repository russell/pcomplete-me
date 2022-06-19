;;; pcmpl-argo-test.el --- Tests for Argo completion  -*- lexical-binding: t; -*-

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


(require 'pcmpl-argo)
(require 'bash-completion-export-utils)

(defconst pcmpl-argo-test-bashinit (format "%s/pcmpl-argo-test.sh"
                                              (cond
                                               (buffer-file-name
                                                (file-name-directory buffer-file-name))
                                               (load-file-name
                                                (file-name-directory load-file-name)))))


(defun rs//bash-complete-argo-subcommands (command)
  ""
  (let ((bash-completion-start-files `(,pcmpl-argo-test-bashinit)))
    (rs//bash-complete-recursive-subcommands command)))


(defun rs//bash-complete-argo-subcommand (command)
  ""
  (let ((bash-completion-start-files `(,pcmpl-argo-test-bashinit)))
    (rs//bash-complete-isolated
     (rs//bash-complete-subcommand command))))

(defun rs//bash-complete-argo-flags (command &optional global-flags)
  ""
  (let ((bash-completion-start-files `(,pcmpl-argo-test-bashinit)))
    (rs//bash-complete-isolated
     (rs//bash-complete-flags command global-flags))))

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
            ,subcommand-flags
            (let ((bash-completion-start-files `(,pcmpl-argo-test-bashinit)))
              (rs//bash-complete-argo-flags ,(mapconcat 'symbol-name command-list " ") ,global-flags))
            :test #'string-equal)
           nil)))
       (ert-deftest ,(intern (mapconcat 'symbol-name `(pcmpl ,@command-list -subcommands) "-")) ()
         (should
          (equal
           (cl-set-difference
            ,subcommand-subcommands
            (rs//bash-complete-argo-subcommand ,(mapconcat 'symbol-name command-list " "))
            :test #'string-equal)
           nil))))))

(pcmpl-me-test (argo) (:inherit-global-flags t))
(pcmpl-me-test (argo archive) (:inherit-global-flags t))
(pcmpl-me-test (argo archive delete) (:inherit-global-flags t))
(pcmpl-me-test (argo archive get) (:inherit-global-flags t))
(pcmpl-me-test (argo archive list) (:inherit-global-flags t))
(pcmpl-me-test (argo auth) (:inherit-global-flags t))
(pcmpl-me-test (argo auth token) (:inherit-global-flags t))
(pcmpl-me-test (argo cluster-template) (:inherit-global-flags t))
(pcmpl-me-test (argo cluster-template create) (:inherit-global-flags t))
(pcmpl-me-test (argo cluster-template delete) (:inherit-global-flags t))
(pcmpl-me-test (argo cluster-template get) (:inherit-global-flags t))
(pcmpl-me-test (argo cluster-template lint) (:inherit-global-flags t))
(pcmpl-me-test (argo cluster-template list) (:inherit-global-flags t))
(pcmpl-me-test (argo completion) (:inherit-global-flags t))
(pcmpl-me-test (argo cron) (:inherit-global-flags t))
(pcmpl-me-test (argo cron create) (:inherit-global-flags t))
(pcmpl-me-test (argo cron delete) (:inherit-global-flags t))
(pcmpl-me-test (argo cron get) (:inherit-global-flags t))
(pcmpl-me-test (argo cron lint) (:inherit-global-flags t))
(pcmpl-me-test (argo cron list) (:inherit-global-flags t))
(pcmpl-me-test (argo cron resume) (:inherit-global-flags t))
(pcmpl-me-test (argo cron suspend) (:inherit-global-flags t))
(pcmpl-me-test (argo delete) (:inherit-global-flags t))
(pcmpl-me-test (argo get) (:inherit-global-flags t))
(pcmpl-me-test (argo lint) (:inherit-global-flags t))
(pcmpl-me-test (argo list) (:inherit-global-flags t))
(pcmpl-me-test (argo logs) (:inherit-global-flags t))
(pcmpl-me-test (argo node) (:inherit-global-flags t))
(pcmpl-me-test (argo resubmit) (:inherit-global-flags t))
(pcmpl-me-test (argo resume) (:inherit-global-flags t))
(pcmpl-me-test (argo retry) (:inherit-global-flags t))
(pcmpl-me-test (argo server) (:inherit-global-flags t))
(pcmpl-me-test (argo stop) (:inherit-global-flags t))
(pcmpl-me-test (argo submit) (:inherit-global-flags t))
(pcmpl-me-test (argo suspend) (:inherit-global-flags t))
(pcmpl-me-test (argo template) (:inherit-global-flags t))
(pcmpl-me-test (argo template create) (:inherit-global-flags t))
(pcmpl-me-test (argo template delete) (:inherit-global-flags t))
(pcmpl-me-test (argo template get) (:inherit-global-flags t))
(pcmpl-me-test (argo template lint) (:inherit-global-flags t))
(pcmpl-me-test (argo template list) (:inherit-global-flags t))
(pcmpl-me-test (argo terminate) (:inherit-global-flags t))
(pcmpl-me-test (argo version) (:inherit-global-flags t))
(pcmpl-me-test (argo wait) (:inherit-global-flags t))
(pcmpl-me-test (argo watch) (:inherit-global-flags t))

(provide 'pcmpl-argo-test)
;;; pcmpl-argo-test.el ends here
