;;; pcmpl-kubectl.el --- Kubernetes completion helpers  -*- lexical-binding: t; -*-

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

(defconst pcmpl-kubectl-commands
  ;; (rs//replace-sexp (rs//bash-complete-kubectl-subcommands "kubectl"))
  '(("annotate")
    ("api-resources")
    ("api-versions")
    ("apply"
     (("edit-last-applied")
      ("set-last-applied")
      ("view-last-applied")))
    ("attach")
    ("auth"
     (("can-i")
      ("reconcile")))
    ("autoscale"
     (("deployment")
      ("replicaset")
      ("replicationcontroller")
      ("statefulset")))
    ("certificate"
     (("approve")
      ("deny")))
    ("cluster-info"
     (("dump")))
    ("completion"
     (("bash")
      ("zsh")))
    ("config"
     (("current-context")
      ("delete-cluster")
      ("delete-context")
      ("delete-user")
      ("get-clusters")
      ("get-contexts")
      ("get-users")
      ("rename-context")
      ("set")
      ("set-cluster")
      ("set-context")
      ("set-credentials")
      ("unset")
      ("use-context")
      ("view")))
    ("cordon")
    ("cp")
    ("create"
     (("clusterrole")
      ("clusterrolebinding")
      ("configmap")
      ("cronjob")
      ("deployment")
      ("ingress")
      ("job")
      ("namespace")
      ("poddisruptionbudget")
      ("priorityclass")
      ("quota")
      ("role")
      ("rolebinding")
      ("secret"
       (("docker-registry")
        ("generic")
        ("tls")))
      ("service"
       (("clusterip")
        ("externalname")
        ("loadbalancer")
        ("nodeport")))
      ("serviceaccount")))
    ("debug")
    ("delete")
    ("describe")
    ("diff")
    ("drain")
    ("edit")
    ("exec")
    ("explain")
    ("expose"
     (("deployment")
      ("pod")
      ("replicaset")
      ("replicationcontroller")
      ("service")))
    ("get")
    ("help"
     (("annotate")
      ("api-resources")
      ("api-versions")
      ("apply"
       (("edit-last-applied")
        ("set-last-applied")
        ("view-last-applied")))
      ("attach")
      ("auth"
       (("can-i")
        ("reconcile")))
      ("autoscale")
      ("certificate"
       (("approve")
        ("deny")))
      ("cluster-info"
       (("dump")))
      ("completion")
      ("config"
       (("current-context")
        ("delete-cluster")
        ("delete-context")
        ("delete-user")
        ("get-clusters")
        ("get-contexts")
        ("get-users")
        ("rename-context")
        ("set")
        ("set-cluster")
        ("set-context")
        ("set-credentials")
        ("unset")
        ("use-context")
        ("view")))
      ("cordon")
      ("cp")
      ("create"
       (("clusterrole")
        ("clusterrolebinding")
        ("configmap")
        ("cronjob")
        ("deployment")
        ("ingress")
        ("job")
        ("namespace")
        ("poddisruptionbudget")
        ("priorityclass")
        ("quota")
        ("role")
        ("rolebinding")
        ("secret"
         (("docker-registry")
          ("generic")
          ("tls")))
        ("service"
         (("clusterip")
          ("externalname")
          ("loadbalancer")
          ("nodeport")))
        ("serviceaccount")))
      ("debug")
      ("delete")
      ("describe")
      ("diff")
      ("drain")
      ("edit")
      ("exec")
      ("explain")
      ("expose")
      ("get")
      ("help")
      ("kustomize")
      ("label")
      ("logs")
      ("options")
      ("patch")
      ("plugin"
       (("list")))
      ("port-forward")
      ("proxy")
      ("replace")
      ("rollout"
       (("history")
        ("pause")
        ("restart")
        ("resume")
        ("status")
        ("undo")))
      ("run")
      ("scale")
      ("set"
       (("env")
        ("image")
        ("resources")
        ("selector")
        ("serviceaccount")
        ("subject")))
      ("taint")
      ("top"
       (("node")
        ("pod")))
      ("uncordon")
      ("version")
      ("wait")))
    ("kustomize")
    ("label")
    ("logs")
    ("options")
    ("patch")
    ("plugin"
     (("list")))
    ("port-forward")
    ("proxy")
    ("replace")
    ("rollout"
     (("history"
       (("daemonset")
        ("deployment")
        ("statefulset")))
      ("pause"
       (("deployment")))
      ("restart"
       (("daemonset")
        ("deployment")
        ("statefulset")))
      ("resume"
       (("deployment")))
      ("status"
       (("daemonset")
        ("deployment")
        ("statefulset")))
      ("undo"
       (("daemonset")
        ("deployment")
        ("statefulset")))))
    ("run")
    ("scale"
     (("deployment")
      ("replicaset")
      ("replicationcontroller")
      ("statefulset")))
    ("set"
     (("env")
      ("image")
      ("resources")
      ("selector")
      ("serviceaccount")
      ("subject")))
    ("taint"
     (("node")))
    ("top"
     (("node")
      ("pod")))
    ("uncordon")
    ("version")
    ("wait"))
  "List of `kubectl' commands")

(defconst pcmpl-kubectl-subcommand-flags nil)

(defun pcmpl-kubectl-get-deepest (search-path)
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

(defun pcmpl-kubectl-get-subcommands (search-path tree)
  ""
  (let ((subtree tree))
    (dolist (command search-path)
      (setq subtree (car (assoc-default command subtree))))
    (mapcar #'car subtree)))

(cl-defmacro pcmpl-subcommand ((subcommand-list &key global-flags flags subcommands) &rest body)
  ""
  (declare (indent 1))
  (let ((subcommand-fn (intern (mapconcat 'symbol-name (append '(pcmpl) subcommand-list) "-")))
        (subcommand-flags (intern (mapconcat 'symbol-name (append '(pcmpl) subcommand-list '(flags)) "-"))))
   `(progn
     (defconst ,subcommand-flags
       (append ,global-flags ,flags))
     (defun ,(intern (mapconcat 'symbol-name (append '(pcmpl) subcommand-list) "-")) ()
       (pcomplete-here* (append
                         (pcmpl-kubectl-get-subcommands (quote ,(mapcar 'symbol-name (cdr subcommand-list)))
                                                        pcmpl-kubectl-commands)
                         ,(unless (null subcommands)
                            `(funcall ,subcommands))
                         ,subcommand-flags))
       ,@body)
     (add-to-list 'pcmpl-kubectl-subcommand-flags
                  (cons ,(if (null (cdr subcommand-list))
                             nil
                           (mapconcat 'symbol-name (cdr subcommand-list) " ")) (quote ,subcommand-fn))))))


(defmacro pcmpl-kubectl--base-flag-file= (matchers)
  ""
  (let ((matchers-list (if (listp matchers) (eval matchers) (list matchers))))
    `((pcomplete-match ,(format "\\`--%s=\\(.*\\)" (regexp-opt matchers-list)) 0)
      (pcomplete-here* (pcomplete-entries)
                       (pcomplete-match-string 1 0)))))

(defmacro pcmpl-kubectl--base-flag-file (matchers)
  ""
  (let ((matchers-list (if (listp matchers) (eval matchers) (list matchers))))
    `((pcomplete-match ,(format "\\`--%s\\'" (regexp-opt matchers-list)) 1)
      (pcomplete-here* (pcomplete-entries)))))

(defmacro pcmpl-kubectl--base-flag-directory= (matchers)
  ""
  (let ((matchers-list (if (listp matchers) (eval matchers) (list matchers))))
    `((pcomplete-match ,(format "\\`--%s=\\(.*\\)" (regexp-opt matchers-list)) 0)
      (pcomplete-here* (pcomplete-dirs)
                       (pcomplete-match-string 1 0)))))

(defmacro pcmpl-kubectl--base-flag-directory (matchers)
  ""
  (let ((matchers-list (if (listp matchers) (eval matchers) (list matchers))))
    `((pcomplete-match ,(format "\\`--%s\\'" (regexp-opt matchers-list)) 1)
      (pcomplete-here* (pcomplete-dirs)))))

(defmacro pcmpl-kubectl--flag-filename= ()
  ""
  `((pcomplete-match "\\`--filename=\\(.*\\)" 0)
    (pcomplete-here* (pcomplete-entries)
                     (pcomplete-match-string 1 0))))

(defmacro pcmpl-kubectl--flag-filename ()
  ""
  `((or (pcomplete-match "\\`-f\\'" 1)
        (pcomplete-match "\\`--filename\\'" 1))
    (pcomplete-here* (pcomplete-entries))))

(defmacro pcmpl-kubectl--flag-kustomize= ()
  ""
  `((pcomplete-match "\\`--kustomize=\\(.*\\)" 0)
    (pcomplete-here* (pcomplete-dirs)
                     (pcomplete-match-string 1 0))))

(defmacro pcmpl-kubectl--flag-kustomize ()
  ""
  `((or (pcomplete-match "\\`-k\\'" 1)
        (pcomplete-match "\\`--kustomize\\'" 1))
    (pcomplete-here* (pcomplete-dirs))))


(defconst kubectl-output-all
  '("json" "yaml" "name" "go-template" "go-template-file" "template" "templatefile" "jsonpath" "jsonpath-as-json" "jsonpath-file"))

(defconst kubectl-output-name
  '("name"))

(defmacro pcmpl-kubectl--flag-output= (outputs)
  ""
  `((pcomplete-match "\\`--output=\\(.*\\)" 0)
    (pcomplete-here* ,outputs
                     (pcomplete-match-string 1 0))))

(defmacro pcmpl-kubectl--flag-output (outputs)
  ""
  `((or (pcomplete-match "\\`-o\\'" 1)
        (pcomplete-match "\\`--output\\'" 1))
    (pcomplete-here* ,outputs)))

(defun pcmpl-kubectl--contexts ()
  (split-string
   (shell-command-to-string "kubectl config get-contexts -o name")))

(pcmpl-subcommand ((kubectl)
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl"))
                   '("--add-dir-header" "--alsologtostderr" "--as=" "--as-group=" "--cache-dir=" "--certificate-authority=" "--client-certificate=" "--client-key=" "--cluster=" "--context=" "--insecure-skip-tls-verify" "--kubeconfig=" "--log-backtrace-at=" "--log-dir=" "--log-file=" "--log-file-max-size=" "--log-flush-frequency=" "--logtostderr" "--match-server-version" "--namespace=" "--one-output" "--password=" "--profile=" "--profile-output=" "--request-timeout=" "--server=" "--skip-headers" "--skip-log-headers" "--stderrthreshold=" "--tls-server-name=" "--token=" "--user=" "--username=" "--v=" "--vmodule=" "--warnings-as-errors" "--as" "--as-group" "--cache-dir" "--certificate-authority" "--client-certificate" "--client-key" "--cluster" "--context" "--kubeconfig" "--log-backtrace-at" "--log-dir" "--log-file" "--log-file-max-size" "--log-flush-frequency" "--namespace" "-n" "--password" "--profile" "--profile-output" "--request-timeout" "--server" "-s" "--stderrthreshold" "--tls-server-name" "--token" "--user" "--username" "--v" "-v" "--vmodule"))
                  (when (pcomplete-match "^--" 0)
                    (cond
                     ((pcomplete-match "\\`--context=\\(.*\\)" 0)
                      (pcomplete-here* (pcmpl-kubectl--contexts)
                                       (pcomplete-match-string 1 0)))
                     ((pcomplete-match "\\`--profile=\\(.*\\)" 0)
                      (pcomplete-here* '("none" "cpu" "heap" "goroutine" "threadcreate" "block" "mutex")
                                       (pcomplete-match-string 1 0)))
                     (pcmpl-kubectl--base-flag-file= '("kubeconfig"
                                                       "certificate-authority"
                                                       "client-certificates"
                                                       "client-key"
                                                       "log-file"
                                                       "profile-output"))
                     (pcmpl-kubectl--base-flag-directory= '("cache-dir" "log-dir"))))
                  (when (pcomplete-match "^-" 1)
                    (cond
                     ((pcomplete-match "\\`--context\\'" 1)
                      (pcomplete-here* (pcmpl-kubectl--contexts)))
                     ((pcomplete-match "\\`--profile\\'" 1)
                      (pcomplete-here* '("none" "cpu" "heap" "goroutine" "threadcreate" "block" "mutex")))
                     ((pcomplete-match "\\`--\\(?:c\\(?:ertificate-authority\\|lient-\\(?:certificates\\|key\\)\\)\\|kubeconfig\\|log-file\\|profile-output\\)\\'" 1)
                      (pcomplete-here*
                       (pcomplete-entries)))
                     ((pcomplete-match "\\`--\\(?:\\(?:cache\\|log\\)-dir\\)\\'" 1)
                      (pcomplete-here*
                       (pcomplete-dirs))))))

(pcmpl-subcommand ((kubectl annotate)
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl annotate" pcmpl-kubectl-flags))
                   '("--all" "--allow-missing-template-keys" "--dry-run" "--field-manager=" "--field-selector=" "--filename=" "--kustomize=" "--list" "--local" "--output=" "--overwrite" "--record" "--recursive" "-R" "--resource-version=" "--selector=" "--show-managed-fields" "--template=" "--field-manager" "--field-selector" "--filename" "-f" "--kustomize" "-k" "--output" "-o" "--resource-version" "--selector" "-l" "--template")))

(pcmpl-subcommand ((kubectl api-versions)
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl api-versions" pcmpl-kubectl-flags))
                   'nil))

(pcmpl-subcommand ((kubectl config)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config" pcmpl-kubectl-flags))
                   'nil))

(pcmpl-subcommand ((kubectl config current-context)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config current-context" pcmpl-kubectl-flags))
                   'nil))

(pcmpl-subcommand ((kubectl config delete-cluster)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config delete-cluster" pcmpl-kubectl-flags))
                   'nil))

(pcmpl-subcommand ((kubectl config delete-context)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config delete-context" pcmpl-kubectl-flags))
                   'nil))

(pcmpl-subcommand ((kubectl config use-context)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config use-context" pcmpl-kubectl-flags))
                   'nil
                   :subcommands (pcmpl-kubectl--contexts)))

(pcmpl-subcommand ((kubectl api-resources)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl api-resources" pcmpl-kubectl-flags))
                   '("--api-group=" "--cached" "--namespaced" "--no-headers" "--output=" "--sort-by=" "--verbs=" "--api-group" "--output" "-o" "--sort-by" "--verbs")))

(pcmpl-subcommand ((kubectl apply)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply" pcmpl-kubectl-flags))
                   '("--all" "--allow-missing-template-keys" "--cascade" "--dry-run" "--field-manager=" "--filename=" "--force" "--force-conflicts" "--grace-period=" "--kustomize=" "--openapi-patch" "--output=" "--overwrite" "--prune" "--prune-whitelist=" "--record" "--recursive" "-R" "--selector=" "--server-side" "--show-managed-fields" "--template=" "--timeout=" "--validate" "--wait" "--field-manager" "--filename" "-f" "--grace-period" "--kustomize" "-k" "--output" "-o" "--prune-whitelist" "--selector" "-l" "--template" "--timeout"))
  (when (pcomplete-match "^--" 0)
    (cond
     (pcmpl-kubectl--flag-filename=)
     (pcmpl-kubectl--flag-kustomize=)))
  (when (pcomplete-match "^-" 1)
    (cond
     (pcmpl-kubectl--flag-filename)
     (pcmpl-kubectl--flag-kustomize))))


(pcmpl-subcommand ((kubectl apply edit-last-applied)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply edit-last-applied" pcmpl-kubectl-flags))
                   '("--allow-missing-template-keys" "--field-manager=" "--filename=" "--kustomize=" "--output=" "--record" "--recursive" "-R" "--show-managed-fields" "--template=" "--windows-line-endings" "--field-manager" "--filename" "-f" "--kustomize" "-k" "--output" "-o" "--template"))
  (when (pcomplete-match "^--" 0)
    (cond
     (pcmpl-kubectl--flag-filename=)
     (pcmpl-kubectl--flag-kustomize=)))
  (when (pcomplete-match "^-" 1)
    (cond
     (pcmpl-kubectl--flag-filename)
     (pcmpl-kubectl--flag-kustomize))))

(pcmpl-subcommand ((kubectl apply set-last-applied)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply set-last-applied" pcmpl-kubectl-flags))
                   '("--allow-missing-template-keys" "--create-annotation" "--dry-run" "--filename=" "--output=" "--show-managed-fields" "--template=" "--filename" "-f" "--output" "-o" "--template"))
  (when (pcomplete-match "^--" 0)
    (cond
     (pcmpl-kubectl--flag-filename=)))
  (when (pcomplete-match "^-" 1)
    (cond
     (pcmpl-kubectl--flag-filename))))

(pcmpl-subcommand ((kubectl attach)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl attach" pcmpl-kubectl-flags))
                   '("--container=" "--pod-running-timeout=" "--quiet" "-q" "--stdin" "-i" "--tty" "-t" "--container" "-c" "--pod-running-timeout"))
  (when (pcomplete-match "^--" 0)
    (cond
     ;; TODO containers
     ))
  (when (pcomplete-match "^-" 1)
    (cond
     )))


(pcmpl-subcommand ((kubectl auth)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl auth" pcmpl-kubectl-flags))
                   'nil))

(pcmpl-subcommand ((kubectl auth can-i)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl auth can-i" pcmpl-kubectl-flags))
                   '("--all-namespaces" "-A" "--list" "--no-headers" "--quiet" "-q" "--subresource=" "--subresource")))

(pcmpl-subcommand ((kubectl auth reconcile)
                   :global-flags pcmpl-kubectl-flags
                   :flags
                   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl auth reconcile" pcmpl-kubectl-flags))
                   '("--allow-missing-template-keys" "--dry-run" "--filename=" "--kustomize=" "--output=" "--recursive" "-R" "--remove-extra-permissions" "--remove-extra-subjects" "--show-managed-fields" "--template=" "--filename" "-f" "--kustomize" "-k" "--output" "-o" "--template"))
  (when (pcomplete-match "^--" 0)
    (cond
     (pcmpl-kubectl--flag-filename=)
     (pcmpl-kubectl--flag-kustomize=)))
  (when (pcomplete-match "^-" 1)
    (cond
     (pcmpl-kubectl--flag-filename)
     (pcmpl-kubectl--flag-kustomize))))


;;
;; PComplete kubectl
;;

(defvar pcmpl-kubectl--namespace nil)
(defvar pcmpl-kubectl--context nil)


;;;###autoload
(defun pcomplete/kubectl ()
  "Completion for `kubectl'"
  (pcmpl-kubectl)
  (let* (current-subcommand)
    (while t
      ;; Look backwards at the last thing, if it's a subcommand, add
      ;; it to the stack
      (when (member (substring-no-properties (pcomplete-arg 1))
                    (pcmpl-kubectl-get-subcommands (reverse current-subcommand)
                                                   pcmpl-kubectl-commands))
        (push (substring-no-properties (pcomplete-arg 1)) current-subcommand))
      (let ((subcompletions (pcmpl-kubectl-get-deepest (reverse current-subcommand))))
        (if (functionp subcompletions)
            (funcall subcompletions)
          (pcomplete-here* nil))))))

(provide 'pcmpl-kubectl)
;;; pcmpl-kubectl.el ends here
