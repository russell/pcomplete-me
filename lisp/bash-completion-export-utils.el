;;; bash-completion-export-utils.el --- Utils to help exporting data from bash  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'bash-completion)

(defvar temp-dir)

(defmacro rs//bash-complete-isolated (&rest body)
  ""
  `(let* ((temp-dir (make-temp-file "kube" t))
          (bash-completion-use-separate-processes t)
          (default-directory temp-dir))
     (unwind-protect
         ,@body
       (delete-directory temp-dir))))

(defun rs//bash-complete-subcommand (subcommand &optional env)
  ""
  (cl-letf* ((process-environment (append env process-environment)))
    (cl-sort
     (seq-filter
      (lambda (e) (string-match "^[^-]" e))
      (mapcar #'string-trim
              (caddr
               (with-temp-buffer
                 (insert (format "%s " subcommand))
                 (bash-completion-dynamic-complete-nocomint (line-beginning-position) (point))))))
     'string-lessp)))

(defun rs//bash-complete-recursive-subcommand (subcommand &optional env)
  ""
  (if (> (length subcommand) 5)
      nil
    (cl-loop
     for command in (rs//bash-complete-subcommand (mapconcat #'identity subcommand " ") env)
     collect
     (let ((subcommands (rs//bash-complete-recursive-subcommand
                         (append subcommand (list command)) env)))
       (if (null subcommands)
           (cons command nil)
         (list command subcommands))))))

(defun rs//bash-complete-recursive-subcommands (subcommand &optional env)
  ""
  (rs//bash-complete-isolated
   (rs//bash-complete-recursive-subcommand (if (listp subcommand) subcommand (list subcommand)) env)))

(defun rs//bash-complete-recursive-subcommand-flags (subcommand)
  ""
  (rs//bash-complete-isolated
   (cl-loop
    for command in (rs//bash-complete-subcommand subcommand)
    collect
    (let ((subcommands (rs//bash-complete-subcommand (format "%s %s" subcommand command))))
      (if (null subcommands)
          (cons command nil)
        (list command subcommands))))))


(defun rs//bash-complete-flags (command-or-subcommand &optional global-flags env)
  ""
  (cl-letf* ((process-environment (append env process-environment)))
   (rs//bash-complete-isolated
    (cl-sort
     (cl-set-difference
      (caddr
       (with-temp-buffer
         (insert (format "%s -" command-or-subcommand))
         (bash-completion-dynamic-complete-nocomint (line-beginning-position) (point))))
      global-flags
      :test #'string-equal)
     'string-lessp))))

(defun rs//replace-sexp (options)
  ""
  (save-excursion
    (kill-sexp)
    (newline)
    (insert (string-trim (format "'%s" (pp-to-string options)))))
  (indent-pp-sexp))


(defun rs//bash-complete-kubectl-subcommands (command)
  ""
  (rs//bash-complete-recursive-subcommands command '("KUBECONFIG=/dev/null")))

(defun rs//bash-complete-kubectl-flags (command &optional global-flags)
  ""
  (rs//bash-complete-recursive-subcommands command global-flags '("KUBECONFIG=/dev/null")))




(provide 'bash-completion-export-utils)
;;; bash-completion-export-utils.el ends here
