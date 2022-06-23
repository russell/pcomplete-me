;;; pcomplete-me.el --- Higher level abstraction for pcomplete  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Russell Sim

;; Author: Russell Sim <russell.sim@gmail.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (pfuture "20220425.1242"))

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

(require 'pfuture)
(require 'cl-lib)
(require 'subr-x)
(require 'pcomplete)

(defvar pcmpl-me--context nil)

(defcustom pcmpl-me-debug nil
  "Debug messages for pcomplete-me completions."
  :group 'pcomplete-me
  :type 'boolean)

(defun pcmpl-me--complete-from-list (&rest things)
  ""
  (if (> (length things) 1)
      things
    (if (listp (car things))
        (car things)
      things)))

(defvar pcmpl-me-completers
  '(:null (lambda ())
          :files pcomplete-entries
          :file-or-directory pcomplete-dirs-or-entries
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
  (cl-sort
   (apply #'append (mapcar (lambda (e) (alist-get :args (pcmpl-me--arg-list e))) pflags))
   'string-lessp))

(defun pcmpl-me--normalise-flags (flags)
  "Remove flags that don't work with pcomplete.

Remove all FLAGS ending with `=' and add implementations of the
same flags without =."
  (mapcar (lambda (flag)
            (let ((flag-end-pos (- (length flag) 1)))
              (if (eq t (compare-strings "=" nil nil
                                         flag flag-end-pos nil))
                 (substring flag 0 flag-end-pos)
               flag)))
          flags))

(defun pcmpl-me--list (items &optional filter-fn)
  ""
  (let ((filtered-items (if filter-fn
                            (funcall filter-fn items)
                          items)))
   (lambda (string pred _action)
     (all-completions string filtered-items pred))))

(defun pcmpl-me--matcher-expression (alist)
  "Generate an expression for matchers from ALIST."
  (let ((matchers
         (cl-loop
          for (key . args) in alist
          for widget-expression = (unless (eql key :args)
                                      (plist-get pcmpl-me-completers key))
          if widget-expression
          collect (cond
                   ((functionp widget-expression)
                    `(,(plist-get pcmpl-me-completers key) ,@args))
                   (t
                    `(quote ,(plist-get pcmpl-me-completers key)))))))
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
  (seq-reduce (lambda (acc element)
                (let ((key (caar acc)))
                  (if (keywordp element)
                      (push (cons element nil) acc)
                    (push element (alist-get key acc))
                    acc)))
              args (list (list :args))))

(defun pcmpl-me--context-symbol (args)
  "Return a symbol to use a the context key.

Take a list of arguments ARGS and return a symbol that can be
used as the context key."
  (intern
   (format ":%s"
           (string-trim
            (cl-reduce  ;; find the longest argument
             (lambda (a b) (if (> (length a) (length b)) a b))
             args)
            ;; Strip any `-' or `=' chars
            "[- \t\n\r]+" "[= \t\n\r]+"))))

(defun pcmpl-me--flag-post-matchers (pflags)
  "Take `PFLAGS' and return post matcher form."

  (cl-loop for pflag in pflags
           for arg-list = (pcmpl-me--arg-list pflag)
           for post-flags = (seq-filter
                             (lambda (s) (string-match-p "[^=]\\'" s))
                             (alist-get :args arg-list))

           for match-expr = (pcmpl-me--matcher-expression arg-list)

           for flag-keyword = (when post-flags
                                (pcmpl-me--context-symbol (alist-get :args arg-list)))

           when  (and match-expr post-flags)
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
  (cl-loop for pflag in pflags
           for arg-list = (pcmpl-me--arg-list pflag)
           for inline-flags = (seq-filter
                               (lambda (s) (string-match-p "=\\'" s))
                               (alist-get :args arg-list))

           for match-expr = (pcmpl-me--matcher-expression arg-list)

           for flag-keyword = (when inline-flags
                                (pcmpl-me--context-symbol (alist-get :args arg-list)))

           when (and match-expr inline-flags)
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
  (let ((widget (plist-get pcmpl-me-completers key)))
    (if widget
        widget
      (error "Completion for %S doesn't exist" key))))

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
    (let* ((command-list (if (listp command) command (cons command nil)))
           (inherit-global-flags (plist-get args :inherit-global-flags))
           (filter-flags (or (plist-get args :filter-flags) (quote #'identity)))
           (flags (eval (plist-get args :flags)))
           (subcommands (plist-get args :subcommands))
           (body (plist-get args :body))
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
             ,(if subcommands
                  `(if (pcomplete-match "\\`-" 0)
                       (pcomplete-here* (completion-table-merge
                                         ,(cond
                                           ((or (functionp subcommands) (functionp (car subcommands)))
                                            `(funcall ,(seq-subseq subcommands 0 2) ,@(cddr subcommands)))
                                           ((listp subcommands)
                                            subcommand-subcommands))
                                         (funcall #'pcmpl-me--list ,subcommand-flags ,filter-flags)
                                         ,(when inherit-global-flags
                                            `(funcall #'pcmpl-me--list ,global-flags ,filter-flags))))
                     ,(cond
                       ((or (functionp subcommands) (functionp (car subcommands)))
                        `(let ((subcommands-result (funcall ,(seq-subseq subcommands 0 2) ,@(cddr subcommands))))
                           (when (or (listp subcommands-result) (functionp subcommands-result))
                             (pcomplete-here* subcommands-result))))
                       ((listp subcommands)
                        `(pcomplete-here* ,subcommand-subcommands))))
                `(pcomplete-here* (append
                                   (funcall #'pcmpl-me--list ,subcommand-flags ,filter-flags)
                                   ,(when inherit-global-flags
                                      `(funcall #'pcmpl-me--list ,global-flags ,filter-flags)))))

             ,@(pcmpl-me--flag-post-matchers flags)
             ,@(pcmpl-me--subcommand-matchers command-list subcommands-list)
             ,(when inherit-global-flags
                `(,global-post-fn))
             ,@body))))))


(defvar pcmpl-me--process-backend 'pcmpl-me--call-process-async-cached
  "Process execution backend.

Must be one of `pcmpl-me--call-process' `pcmpl-me--call-process-cached'
or `pcmpl-me--call-process-async-cached'")

(defun pcmpl-me--call-process (program &rest args)
  "Call process in temporary buffer.

PROGRAM is the binary to be executed, and arguments ARGS to pass
to it."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defvar pcmpl-me--cache-size 10
  "Size of the cache, set to 0 to disable the cache.
Disabling the cache is useful on non-incremental UIs like default completion or
for performance profiling of the annotators.")

(defvar pcmpl-me--cache-expiry 120
  "Expiry time in seconds.")

(defvar pcmpl-me--cache
  (cons nil (make-hash-table :test #'equal
                             :size pcmpl-me--cache-size))
  "The cache, pair of list and hashtable.")

(defun pcmpl-me--cache-reset ()
  "Reset the cache."
  (setq pcmpl-me--cache (and pcmpl-me--cache (> pcmpl-me--cache-size 0)
                             (cons nil (make-hash-table :test #'equal
                                                        :size pcmpl-me--cache-size)))))

(defun pcmpl-me--cache-expired-p (time)
  "Return T if TIME is expired."
  (time-less-p time (current-time)))

(defun pcmpl-me--cache-expire-oldest (cache cache-size)
  "Expire oldest entry from the CACHE hash table.

Will keep the hash table from exceeding CACHE-SIZE."
  (when (>= (hash-table-count cache) cache-size)
    (let ((end (last (car cache) 2)))
      (remhash (cadr end) cache)
      (setcdr end nil))))

(defun pcmpl-me--call-process-cached (args)
  "Cached results of subprocesses called with ARGS.

The CACHE keeps around the last `pcmpl-me--cache-size' computed
annotations. Will refresh items if older than the
`pcmpl-me--cache-expiry'
"
  (let* ((cache (cdr pcmpl-me--cache)))
    (cl-destructuring-bind (expiry entry) (or (gethash args cache) '(nil nil))
      ;; if entry is null or expired create a new entry
      (if (or (null entry) (pcmpl-me--cache-expired-p expiry))
          (cl-destructuring-bind (code result)
              (apply #'pcmpl-me--call-process args)

            ;; If command is a success then update the cache.
            (when (= code 0)
              (push args (car pcmpl-me--cache))
              (puthash args
                       (list (time-add (current-time) pcmpl-me--cache-expiry)
                             (list code result))
                       cache)

              ;; Remove old hash entries if we run out of space
              (pcmpl-me--cache-expire-oldest pcmpl-me--cache pcmpl-me--cache-size))

            (list code result))
        entry))))


(defvar pcmpl-me--parallel-pool 5)
(defvar pcmpl-me--parallel-processes
  (cons nil (make-hash-table :test #'equal
                             :size pcmpl-me--parallel-pool))
  "The cache, pair of list and hashtable of processes.")


(defun pcmpl-me--call-process-async-cached (args)
  "Cached results of subprocesses called with ARGS.

The CACHE keeps around the last `pcmpl-me--cache-size' computed
annotations. Will refresh items if older than the
`pcmpl-me--cache-expiry'
"
  (let ((cache (cdr pcmpl-me--cache)))
    (cl-destructuring-bind (expiry entry) (or (gethash args cache) '(nil nil))
      ;; if entry is null or expired create a new entry

      (if (or (null entry) (pcmpl-me--cache-expired-p (time-add expiry -10))) ; refresh cache 10s before expiry
          (progn
            (unless (gethash args (cdr pcmpl-me--parallel-processes))
              (if (<=  (hash-table-size (cdr pcmpl-me--parallel-processes))
                       (hash-table-count (cdr pcmpl-me--parallel-processes)))
               (let ((process
                      (pfuture-callback args
                        :name (format "pcomplete-me %s" args)
                        :on-success '(lambda (process status buffer)
                                       (remhash args (cdr pcmpl-me--parallel-processes))  ; Clear entry from proc hash
                                       (when (= (process-exit-status process) 0)
                                         ;; Remove old hash entries if we run out of space
                                         (pcmpl-me--cache-expire-oldest cache pcmpl-me--cache-size)

                                         (push args (car pcmpl-me--cache))
                                         (puthash args
                                                  (list (time-add (current-time) pcmpl-me--cache-expiry)
                                                        (list (process-exit-status process)
                                                              (with-current-buffer buffer (buffer-string))))
                                                  cache)))
                        :on-error '(lambda (process status buffer)
                                     (remhash args pcmpl-me--parallel-processes)
                                     (message "Error %s\n%s\n%s" process status
                                              (with-current-buffer buffer (buffer-string)))))))

                 (puthash args (list (time-add (current-time) pcmpl-me--cache-expiry) process) (cdr pcmpl-me--parallel-processes)))
               (message "pcmpl-me--call-process-async-cached ERROR too many autocomplete background processes")))
            entry)
        entry))))

(defun pcmpl-me--call (&rest args)
  "Call subprocess with ARGS."
  (cl-destructuring-bind (code result)
      (let ((time (current-time)))
        (prog1
            (funcall pcmpl-me--process-backend args)
          (when pcmpl-me-debug
           (message "pcmpl-me--call (%.06f) %S" (float-time (time-since time)) args))))
    (if (= code 0)
        result
      (message (string-trim result))
      "")))

(provide 'pcomplete-me)
;;; pcomplete-me.el ends here
