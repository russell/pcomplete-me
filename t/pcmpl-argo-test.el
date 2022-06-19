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

(require 'thunk)

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
  (cl-flet ((test-name (&rest args) (intern (mapconcat 'symbol-name args "-"))))
    (let* ((command-list (if (listp command) command (cons command nil)))
           (global-command (car command-list))
           (global-flags (intern (mapconcat 'symbol-name `(pcmpl ,global-command -global-flags) "-")))
           (subcommand-flags (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -flags) "-")))
           (subcommand-subcommands (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -subcommands) "-"))))
      `(progn
         (thunk-let ((actual-flags (let ((bash-completion-start-files `(,pcmpl-argo-test-bashinit)))
                                     (rs//bash-complete-argo-flags ,(mapconcat 'symbol-name command-list " ") ,global-flags)))
                     (actual-subcommands (rs//bash-complete-argo-subcommand ,(mapconcat 'symbol-name command-list " "))))

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

(pcmpl-me-test (argo) (:inherit-global-flags t))
(pcmpl-me-test (argo archive) (:inherit-global-flags t))
(pcmpl-me-test (argo archive delete) (:inherit-global-flags t))
(pcmpl-me-test (argo archive get) (:inherit-global-flags t))
(pcmpl-me-test (argo archive list) (:inherit-global-flags t))
(pcmpl-me-test (argo archive list-label-keys) (:inherit-global-flags t))
(pcmpl-me-test (argo archive list-label-values) (:inherit-global-flags t))
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
(pcmpl-me-test (argo executor-plugin) (:inherit-global-flags t))
(pcmpl-me-test (argo executor-plugin build) (:inherit-global-flags t))
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
