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
(require 'subr-x)
(require 'pcomplete)

(defvar pcmpl-me--context nil)

(defun pcmpl-me--complete-from-list (&rest things)
  ""
  (if (> (length things) 1)
      things
    (if (listp (car things))
        (car things)
      things)))

(defvar pcmpl-me-completers
  '(:files pcomplete-entries
           :dirs pcomplete-dirs
           :list pcmpl-me--complete-from-list))

(defun pcmpl-me--context-get (key &optional context)
  ""
  (cdr (assoc key (or context pcmpl-me--context))))

(defun pcmpl-me--context-set (key value &optional context)
  ""
  (if context
      (push (cons key value) context)
    (push (cons key value) pcmpl-me--context)))

(defun pcmpl-me--flags (pflags)
  "Return the combine all the flags from `PFLAGS'."
  (apply #'append (mapcar #'car pflags)))

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

(defun pcmpl-me--arg-list (args)
  "Take pseudo plist as ARGS and return an alist.

For example:
  (pcmpl-me--arg-list '(\"--verbose\" \"--verbose=\" :foobar \"barfoo\"))
  -> '((:foobar \"barfoo\")
       (:args \"--verbose=\" \"--verbose\"))"
  (let (args-alist)
   (cl-loop for element in `(:args ,@args)
            when (keywordp element)
            for key = element
            else
            do (if (alist-get key args-alist)
                     (push element (alist-get key args-alist))
                 (push (cons key (cons element nil)) args-alist))
            finally
            do (unless (alist-get key args-alist)
                 (push (cons key nil) args-alist)))
   args-alist))

(defun pcmpl-me--flag-post-matchers (pflags)
  "Take `PFLAGS' and return post matcher form."
  (cl-loop for (flags . matchers) in pflags
           for post-flags = (seq-filter (lambda (s) (string-match-p "[^=]\\'" s)) flags)

           when matchers
           for match-expr = (pcmpl-me--matcher-expression matchers)

           when flags
           for flag-keyword = (intern (format ":%s" (string-trim (car flags) "[- \t\n\r]+" "[= \t\n\r]+")))

           when  (and matchers post-flags)
           collect `((pcomplete-match ,(format "\\`%s\\'" (regexp-opt-group post-flags nil t)) 1)
                     (pcomplete-here* ,match-expr)
                     (pcmpl-me--context-set ,flag-keyword (pcomplete-arg 1)))
           into conds

           finally return `(,@(unless (null conds)
                                `((when (pcomplete-match "\\`-" 1)
                                    (cond
                                     ,@conds)))))))

(defun pcmpl-me--flag-inline-matchers (pflags)
  "Take `PFLAGS' and return inline matcher form."
  (cl-loop for (flags . matchers) in pflags
           for inline-flags = (seq-filter (lambda (s) (string-match-p "=\\'" s)) flags)

           when matchers
           for match-expr = (pcmpl-me--matcher-expression matchers)

           when inline-flags
           for flag-keyword = (intern (format ":%s" (string-trim  (car flags) "[- \t\n\r]+" "[= \t\n\r]+")))

           when (and matchers inline-flags)
           collect `((pcomplete-match ,(format "\\`%s\\(.*\\)" (regexp-opt-group inline-flags nil t)) 0)
                     (pcomplete-here* ,match-expr
                                      (pcomplete-match-string 1 0) t)
                     (pcomplete-match ,(format "\\`%s\\(.*\\)" (regexp-opt-group inline-flags nil t)) 1)
                     (pcmpl-me--context-set ,flag-keyword (pcomplete-match-string 1)))
           into inline-conds

           finally return `(,@(unless (null inline-conds)
                                `((when (pcomplete-match "\\`-" 0)
                                    (cond
                                     ,@inline-conds)))))))

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

(defun pcmpl-me-set-completion-widget (key widget)
  ""
  (plist-put pcmpl-me-completers key widget))

(defun pcmpl-me-get-completion-widget (key)
  ""
  (plist-get pcmpl-me-completers key))

(defmacro pcmpl-me-global-args (name &rest args)
  ""
  (declare (indent 1))
  (cl-flet ((intern-symbol (args) (intern (mapconcat 'symbol-name args "-"))))
    (let* ((flags (eval (plist-get args :flags)))
           (body (plist-get args :body))
           (global-inline-fn (intern-symbol `(pcmpl ,name -global-inline-matchers)))
           (global-post-fn (intern-symbol `(pcmpl ,name -global-post-matchers)))
           (global-flags (intern-symbol `(pcmpl ,name -global-flags))))
      `(progn
         (defconst ,global-flags (quote ,(pcmpl-me--flags flags)))

         (defun ,global-inline-fn ()
           ,@(pcmpl-me--flag-inline-matchers flags)
           ,@body)

         (defun ,global-post-fn ()
           ,@(pcmpl-me--flag-post-matchers flags)
           ,@body)))))

(defmacro pcmpl-me-command (command &rest args)
  "Declare PCompletion for a command by specifying configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (pcompl-me-command command
     [:keyword [option]]...)

COMMAND can be either a list with subcommands or a symbol.

:inherit-global-flags Should the command inherit commands from a global set.
:flags                A list of flags
:subcommands          A list of subcommands or a function that returns
                      subcommands."
  (declare (indent 1))
  (cl-flet ((intern-symbol (args) (intern (mapconcat 'symbol-name args "-"))))
    (let* ((inherit-global-flags (plist-get args :inherit-global-flags))
           (flags (eval (plist-get args :flags)))
           (subcommands (plist-get args :subcommands))
           (subcommands-fn (plist-get args :subcommands-fn))
           (body (plist-get args :body))
           (command-list (if (listp command) command (cons command nil)))
           (global-command (car command-list))
           (subcommands-list (when (and (listp subcommands)
                                        (not (functionp (car subcommands)))
                                        (not (eq (car subcommands) 'lambda)))
                               (eval subcommands)))
           (global-inline-fn (intern-symbol `(pcmpl ,global-command -global-inline-matchers)))
           (global-post-fn (intern-symbol `(pcmpl ,global-command -global-post-matchers)))
           (global-flags (intern-symbol `(pcmpl ,global-command -global-flags)))
           (cmpl-command (intern-symbol `(pcmpl ,@command-list)))
           (subcommand-flags (intern-symbol `(pcmpl ,@command-list -flags)))
           (subcommand-subcommands (intern-symbol `(pcmpl ,@command-list -subcommands))))
      `(progn
         (defconst ,subcommand-flags (quote ,(pcmpl-me--flags flags)))
         (defconst ,subcommand-subcommands ,(unless (or (functionp subcommands)
                                                        (functionp (car subcommands)))
                                              subcommands))

         (defun ,cmpl-command ()
           (while t
             ,@(pcmpl-me--flag-inline-matchers flags)
             ,(when inherit-global-flags
                `(,global-inline-fn))
             ,(if (or subcommands subcommands-fn)
                  `(if (pcomplete-match "\\`-" 0)
                       (pcomplete-here* (append
                                         ,(cond
                                           ((or (functionp subcommands) (functionp (car subcommands)))
                                            `(funcall ,subcommands))
                                           ((listp subcommands)
                                            subcommand-subcommands))
                                         ,subcommand-flags
                                         ,@(when inherit-global-flags
                                             `(,global-flags))))
                     ,(if subcommands-fn
                          `(funcall ,subcommands-fn)
                        `(pcomplete-here* ,(cond
                                           ((or (functionp subcommands) (functionp (car subcommands)))
                                            `(funcall ,subcommands))
                                           ((listp subcommands)
                                            subcommand-subcommands)))))
                `(pcomplete-here* (append
                                   ,subcommand-flags
                                   ,@(when inherit-global-flags
                                       `(,global-flags)))))

             ,@(pcmpl-me--flag-post-matchers flags)
             ,@(pcmpl-me--subcommand-matchers command-list subcommands-list)
             ,(when inherit-global-flags
                `(,global-post-fn))
             ,@body))))))

(provide 'pcomplete-me)
;;; pcomplete-me.el ends here
