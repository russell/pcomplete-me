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
(require 'subr-x)
(require 'bash-completion)

(defvar temp-dir)

(defmacro rs//bash-complete-isolated (&rest body)
  ""
  `(let* ((temp-dir (make-temp-file "kube" t))
          (bash-completion-use-separate-processes t)
          (default-directory temp-dir))
     (unwind-protect
         (progn
          (bash-completion-reset-all)
          ,@body)
       (delete-directory temp-dir))))

(defun rs//bash-complete-subcommand (subcommand)
  ""
  (cl-sort
   (seq-filter
    (lambda (e) (string-match "^[^-]" e))
    (mapcar #'string-trim
            (caddr
             (with-temp-buffer
               (insert (format "%s " subcommand))
               (bash-completion-dynamic-complete-nocomint (line-beginning-position) (point))))))
   'string-lessp))

(defun rs//bash-complete-recursive-subcommand (subcommand)
  ""
  (if (> (length subcommand) 5)
      nil
    (cl-loop
     for command in (rs//bash-complete-subcommand (mapconcat #'identity subcommand " "))
     collect
     (let ((subcommands (rs//bash-complete-recursive-subcommand
                         (append subcommand (list command)))))
       (if (null subcommands)
           (cons command nil)
         (cons command subcommands))))))

(defun rs//bash-complete-recursive-subcommands (subcommand)
  ""
  (rs//bash-complete-isolated
   (rs//bash-complete-recursive-subcommand (if (listp subcommand) subcommand (list subcommand)))))

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


(defun rs//bash-complete-flags (command-or-subcommand &optional global-flags)
  ""
  (rs//bash-complete-isolated
   (cl-sort
    (cl-set-difference
     (caddr
      (with-temp-buffer
        (insert (format "%s -" command-or-subcommand))
        (bash-completion-dynamic-complete-nocomint (line-beginning-position) (point))))
     global-flags
     :test #'string-equal)
    'string-lessp)))

(defun rs//replace-sexp (options)
  ""
  (save-excursion
    (kill-sexp)
    (newline)
    (insert (string-trim (format "'%s" (pp-to-string options)))))
  (indent-pp-sexp))

(defun rs//subcommand-tree-to-commands (tree path)
  ;; TODO this doesn't correctly capture all subcommand's it misses
  ;; the intermediate ones.
  (if (null tree)
      path
    (cl-loop
     for p in tree
     for key = (car p)

     do (message "path %S" path)

     if (null tree)
     do (message "null tree")

     if (null (assoc-default key tree #'string-equal))
     collect (format "(pcmpl-me-test (%s) (:inherit-global-flags t))"
                     (mapconcat #'identity (reverse (cons key path)) " "))
     else
     collect (mapcar
              (lambda (e) (command-things-now e (cons key path)))
              (assoc-default key tree #'string-equal)))))

(defun rs//group-flags (flags)
  ""
  (mapcar
   #'cdr
   (seq-group-by (lambda (e) (string-trim e nil "=")) flags)))

(defmacro rs//generate-pcmpl-me-command (command &optional global-flags)
  ""
  (let ((subcommands (rs//bash-complete-kubectl-subcommand command))
        (flags (rs//group-flags (rs//bash-complete-flags command global-flags))))
   `(pcmpl-me-command
        :inherit-global-flags ,(when pcmpl-kubectl--global-flags t)
        :flags ,flags
        :subcommands ,subcommands)))

(defun rs//bash-complete-kubectl-subcommands (command)
  ""
  (rs//bash-complete-recursive-subcommands command))

(defun rs//bash-complete-kubectl-flags (command &optional global-flags)
  ""
  (rs//bash-complete-flags command global-flags))

(provide 'bash-completion-export-utils)
;;; bash-completion-export-utils.el ends here
