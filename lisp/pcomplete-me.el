;;; pcomplete-me.el --- Higher level abstraction for pcomplete  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Russell Sim

;; Author: Russell Sim <russell.sim@gmail.com>
;; Keywords: convenience

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
(require 'pcomplete)

(defmacro pcmpl-me--base-flag-file= (matchers)
  ""
  (let ((matchers-list (if (listp matchers) (eval matchers) (list matchers))))
    `((pcomplete-match ,(format "\\`--%s=\\(.*\\)" (regexp-opt matchers-list)) 0)
      (pcomplete-here* (pcomplete-entries)
                       (pcomplete-match-string 1 0)))))

(defmacro pcmpl-me--base-flag-file (matchers)
  ""
  (let ((matchers-list (if (listp matchers) (eval matchers) (list matchers))))
    `((pcomplete-match ,(format "\\`--%s\\'" (regexp-opt matchers-list)) 1)
      (pcomplete-here* (pcomplete-entries)))))

(defmacro pcmpl-me--base-flag-directory= (matchers)
  ""
  (let ((matchers-list (if (listp matchers) (eval matchers) (list matchers))))
    `((pcomplete-match ,(format "\\`--%s=\\(.*\\)" (regexp-opt matchers-list)) 0)
      (pcomplete-here* (pcomplete-dirs)
                       (pcomplete-match-string 1 0)))))

(defmacro pcmpl-me--base-flag-directory (matchers)
  ""
  (let ((matchers-list (if (listp matchers) (eval matchers) (list matchers))))
    `((pcomplete-match ,(format "\\`--%s\\'" (regexp-opt matchers-list)) 1)
      (pcomplete-here* (pcomplete-dirs)))))

(defvar pcmpl-me-completers
  '(:files pcomplete-entries
           :dirs pcomplete-dirs
           :list list))

(defun pcmpl-me--get-deepest (search-path collected-subcommand)
  ""
  (let (
        ;; collect all the possible subcommands so we can search for
        ;; the farthest one.
        (searches (reverse
                   (cl-loop for s in search-path
                            collect s into ss
                            collect (mapconcat 'identity ss " ")))))
    (assoc-default (seq-find
                    (lambda (e) (assoc-default e collected-subcommand)) searches)
                   collected-subcommand)))

(defun pcmpl-me--flags (pflags)
  "Return the combine all the flags from `PFLAGS'."
  (apply #'append (mapcar #'car pflags)))

;; (apply #'append '(("--profile" "--profile=")))

;; (pcmpl-me-global-args lorem (:flags '((("--profile" "--profile=") . (:files)))))

(defun pcmpl-me--matcher-expression (plist)
  "Generate an expression for matchers from plist"
  (when (not (keywordp (car plist)))
    (error "list must start with a keyword"))
  (let (matchers pkey args)
    (dolist (i plist)
      (if (keywordp i)
          (progn
            (when pkey
              (push `(,(plist-get pcmpl-me-completers pkey) ,@args) matchers))
            (setq pkey i args nil))
        (push i args)))
    ;; Add the final entry to the list
    (when pkey
      (push `(,(plist-get pcmpl-me-completers pkey) ,@args) matchers))
    (cond
     ((> (length matchers) 1)
      `(append ,@matchers))
     (t (car matchers)))))

(defun pcmpl-me--flag-matchers (pflags)
  "Take `PFLAGS' and return matcher form."
  (cl-loop for (flags . matchers) in pflags
           for inline-flags = (seq-filter (lambda (s) (string-match-p "=\\'" s)) flags)
           for post-flags = (seq-filter (lambda (s) (string-match-p "[^=]\\'" s)) flags)

           when matchers
           for match-expr = (pcmpl-me--matcher-expression matchers)

           when (and matchers inline-flags)
           collect `((pcomplete-match ,(format "\\`%s\\(.*\\)" (regexp-opt-group inline-flags nil t)) 0)
                     (pcomplete-here* ,match-expr
                                      (pcomplete-match-string 1 0)))
           into inline-conds

           when  (and matchers post-flags)
           collect `((pcomplete-match ,(format "\\`%s\\'" (regexp-opt-group post-flags nil t)) 1)
                     (pcomplete-here* ,match-expr))
           into conds

           finally return `(,@(unless (null inline-conds)
                                `((when (pcomplete-match "^-" 0)
                                    (cond
                                     ,@inline-conds))))
                            ,@(unless (null conds)
                                `((when (pcomplete-match "^-" 1)
                                    (cond
                                     ,@conds)))))))

(defun pcmpl-me--to-string (object)
  "Convert `OBJECT' to a string."
  (cond
   ((symbolp object)
    (symbol-name object))
   (t
    object)))

(defun pcmpl-me--subcommand-matchers (command subcommands)
  "Create a cond matcher for `COMMAND' and each subcommand from `SUBCOMMANDS'."
  (cl-loop for subcommand in subcommands
           collect `((pcomplete-match ,(format "\\`%s\\'" subcommand) 1)
                     (,(intern (mapconcat 'pcmpl-me--to-string `(pcmpl ,@command ,subcommand) "-"))))
           into conds

           finally return (when conds `((cond ,@conds)))))

(cl-defmacro pcmpl-me-global-args (name (&key flags) &rest body)
  ""
  (declare (indent 1))
  (let* ((flags (eval flags))
         (global-fn (intern (mapconcat 'symbol-name `(pcmpl ,name -global-matchers) "-")))
         (global-flags (intern (mapconcat 'symbol-name `(pcmpl ,name -global-flags) "-"))))
    `(progn
       (defconst ,global-flags (quote ,(pcmpl-me--flags flags)))

       (defun ,global-fn ()
         ,@(pcmpl-me--flag-matchers flags)
         ,@body))))

;; (macroexpand
;;  '(pcmpl-me-global-args lorem (:flags ((("--profile" "--profile=") . (:files))))))

(cl-defmacro pcmpl-me-command (command (&key inherit-global-flags flags subcommands) &rest body)
  ""
  (declare (indent 1))
  (let* ((flags (eval flags))
         (command-list (if (listp command) command (cons command nil)))
         (global-command (car command-list))
         (subcommands-list (when (and (listp subcommands)
                                      (not (functionp (car subcommands)))
                                      (not (eq (car subcommands) 'lambda)))
                             (eval subcommands)))
         (global-fn (intern (mapconcat 'symbol-name `(pcmpl ,global-command -global-matchers) "-")))
         (global-flags (intern (mapconcat 'symbol-name `(pcmpl ,global-command -global-flags) "-")))
         (subcommand-fn (intern (mapconcat 'symbol-name `(pcmpl ,@command-list) "-")))
         (subcommand-flags (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -flags) "-")))
         (subcommand-subcommands (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -subcommands) "-"))))
    `(progn
       (defconst ,subcommand-flags (quote ,(pcmpl-me--flags flags)))
       (defconst ,subcommand-subcommands ,(when (listp subcommands) subcommands))

       (defun ,subcommand-fn ()
         (while t
           (pcomplete-here* (append
                             ,(cond
                               ((or (functionp subcommands) (functionp (car subcommands)))
                                `(funcall ,subcommands))
                               ((listp subcommands)
                                subcommand-subcommands))
                             ,@(when inherit-global-flags
                                 `(,global-flags))
                             ,subcommand-flags))
           ,@(pcmpl-me--flag-matchers flags)
           ,@(pcmpl-me--subcommand-matchers command-list subcommands-list)
           ,(when inherit-global-flags
              `(,global-fn))
           ,@body)))))

;; (macroexpand
;;  '(pcmpl-me-command (lorem
;;                      :flags ((("--profile" "--profile=") . (:files)))
;;                      :subcommands car)))

;; (macroexpand '(pcmpl-me-subcommand ((lorem ipsum)
;;                        :flags ((("--profile" "--profile=") . (:files))))))

(provide 'pcomplete-me)
;;; pcomplete-me.el ends here
