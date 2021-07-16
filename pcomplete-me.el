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

(defconst pcmpl-me-completers
  '(:files pcomplete-entries
           :dirs pcomplete-dirs
           :list list))

(defun pcmpl-me--get-deepest (search-path )
  ""
  (let (
        ;; collect all the possible subcommands so we can search for
        ;; the farthest one.
        (searches (reverse
                   (cl-loop for s in search-path
                            collect s into ss
                            collect (mapconcat 'identity ss " ")))))
    (assoc-default (seq-find
                    (lambda (e) (assoc-default e pcmpl-kubectl-subcommand-flags)) searches)
                   pcmpl-kubectl-subcommand-flags)))

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
    (if (> (length matchers) 1)
        `(append ,@matchers)
      (car matchers))))

(defun pcmpl-me--flag-matchers (pflags)
  ""
  (cl-loop for (flags . matchers) in pflags
           for inline-flags = (seq-filter (lambda (s) (string-match-p "=\\'" s)) flags)
           for post-flags = (seq-filter (lambda (s) (string-match-p "[^=]\\'" s)) flags)

           when matchers
           for match-expr = (pcmpl-me--matcher-expression matchers)

           when matchers
           collect `((pcomplete-match ,(format "\\`%s\\(.*\\)" (regexp-opt-group inline-flags nil t)) 0)
                     (pcomplete-here* ,match-expr
                                     (pcomplete-match-string 1 0)))
           into inline-conds

           when matchers
           collect `((pcomplete-match ,(format "\\`%s\\'" (regexp-opt-group post-flags nil t)) 1)
                     (pcomplete-here* ,match-expr))
           into conds

           finally return `(progn
                     (when (pcomplete-match "^-" 0)
                       (cond
                        ,@inline-conds))
                     (when (pcomplete-match "^-" 1)
                       (cond
                        ,@conds)))))

(cl-defmacro pcmpl-me-command ((name &key subcommands) &rest body)
  `(progn
     ,(when subcommands `(defconst ,(intern (mapconcat 'symbol-name `(pcmpl ,name subcommands) "-"))))))

;; (macroexpand )

;; (pcmpl-me-command (com :subcommands t))

(cl-defmacro pcmpl-me-subcommand ((subcommand-list &key global-flags flags subcommands) &rest body)
  ""
  (declare (indent 1))
  (let* ((collected-subcommands (intern (mapconcat 'symbol-name `(pcmpl ,(car subcommand-list) -subcommands) "-")))
         (subcommand-fn (intern (mapconcat 'symbol-name `(pcmpl ,@subcommand-list) "-")))
         (subcommand-flags (intern (mapconcat 'symbol-name `(pcmpl ,@subcommand-list flags) "-"))))
    `(progn
       (defconst ,subcommand-flags
         (append ,global-flags ,(pcmpl-me--flags flags)))

       (defun ,(intern (mapconcat 'symbol-name (append '(pcmpl) subcommand-list) "-")) ()
         (pcomplete-here* (append
                           ,(cond
                             ((functionp subcommands)
                              `(funcall ,subcommands))
                             ((listp subcommands)
                              subcommands))
                           ,subcommand-flags))
         ,(pcmpl-me--flag-matchers flags)
         ,@body)

       (when (boundp ,collected-subcommands)
         (defconst ,collected-subcommands nil))
       ;; Add to the list of subcommands
       (add-to-list ,collected-subcommands
                    (cons ,(if (null (cdr subcommand-list))
                               nil
                             (mapconcat 'symbol-name (cdr subcommand-list) " ")) (quote ,subcommand-fn))))))


;; (macroexpand '(pcmpl-me-subcommand ((lorem ipsum)
;;                        :flags ((("--profile" "--profile=") . (:files))))))

(provide 'pcomplete-me)
;;; pcomplete-me.el ends here
