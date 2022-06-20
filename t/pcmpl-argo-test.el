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

(require 'pcomplete-me-test)
(require 'pcmpl-argo)
(require 'bash-completion-export-utils)

(defconst pcmpl-argo-test-bashinit (list
                                    (format "%s/pcmpl-argo-test.sh"
                                            (cond
                                             (buffer-file-name
                                              (file-name-directory buffer-file-name))
                                             (load-file-name
                                              (file-name-directory load-file-name))))))

(defun pcmpl-me-argo-test-completion (command-list type &optional global-flags)
  "COMMAND-LIST as a representation of the command"
  (let ((command (mapconcat 'symbol-name command-list " "))
        (bash-completion-start-files pcmpl-argo-test-bashinit))
    (rs//bash-complete-isolated
     (cl-case type
       (:flags (rs//bash-complete-flags command global-flags))
       (:subcommand (rs//bash-complete-subcommand command))))))


(pcmpl-me-test (argo) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo archive) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo archive delete) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo archive get) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo archive list) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo archive list-label-keys) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo archive list-label-values) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo auth) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo auth token) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cluster-template) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cluster-template create) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cluster-template delete) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cluster-template get) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cluster-template lint) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cluster-template list) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo completion) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cron) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cron create) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cron delete) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cron get) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cron lint) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cron list) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cron resume) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo cron suspend) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo delete) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo executor-plugin) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo executor-plugin build) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo get) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo lint) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo list) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo logs) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo node) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo resubmit) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo resume) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo retry) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo server) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo stop) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo submit) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo suspend) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo template) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo template create) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo template delete) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo template get) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo template lint) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo template list) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo terminate) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo version) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo wait) (pcmpl-me-argo-test-completion :inherit-global-flags t))
(pcmpl-me-test (argo watch) (pcmpl-me-argo-test-completion :inherit-global-flags t))

(provide 'pcmpl-argo-test)
;;; pcmpl-argo-test.el ends here
