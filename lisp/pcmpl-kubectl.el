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
(require 'pcomplete-me)

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

(defconst kubectl-output-all
  '("json" "yaml" "name" "go-template" "go-template-file" "template" "templatefile" "jsonpath" "jsonpath-as-json" "jsonpath-file"))

(defconst kubectl-output-name
  '("name"))

(defun pcmpl-kubectl--complete (type)
  ""
  (split-string
   (shell-command-to-string
    (format "kubectl config view -o template --template=\"{{ range .%s}}{{ .name }}\n{{end}}\"" type))))

(eval-when-compile
 (plist-put pcmpl-me-completers :kubernetes-context (lambda () (pcmpl-kubectl--complete "contexts")))
 (plist-put pcmpl-me-completers :kubernetes-user (lambda () (pcmpl-kubectl--complete "users")))
 (plist-put pcmpl-me-completers :kubernetes-cluster (lambda () (pcmpl-kubectl--complete "clusters"))))

(pcmpl-me-global-args kubectl
  (:flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl"))
   '((("--add-dir-header"
       "--alsologtostderr"
       "--one-output"
       "--logtostderr"
       "--match-server-version"
       "--skip-headers"
       "--skip-log-headers"
       "--warnings-as-errors"))
     (("--as"
       "--as="
       "--as-group"
       "--as-group="
       "-h"
       "--help"
       "--insecure-skip-tls-verify"
       "--log-backtrace-at"
       "--log-backtrace-at="
       "--log-file-max-size"
       "--log-file-max-size="
       "--log-flush-frequency"
       "--log-flush-frequency="
       "-n"
       "--namespace"
       "--namespace="
       "--password"
       "--password="
       "--profile-output"
       "--profile-output="
       "--request-timeout"
       "--request-timeout="
       "-s"
       "--server"
       "--server="
       "--stderrthreshold"
       "--stderrthreshold="
       "--tls-server-name"
       "--tls-server-name="
       "--token"
       "--token="
       "--username"
       "--username="
       "--vmodule"
       "--vmodule="
       "-v"
       "--v"
       "--v="))
     (("--profile" "--profile=") . (:list "none" "cpu" "heap" "goroutine" "threadcreate" "block" "mutex"))
     (("--cluster" "--cluster=") . (:kubernetes-cluster))
     (("--user" "--user=") . (:kubernetes-user))
     (("--context" "--context=") . (:kubernetes-context))
     (("--log-file" "--log-file=" "--kubeconfig" "--kubeconfig="
       "--certificate-authority" "--certificate-authority="
       "--client-certificate" "--client-certificate="
       "--client-key" "--client-key=") . (:files))
     (("--log-dir" "--log-dir="
       "--cache-dir" "--cache-dir=") . (:dirs)))))

(pcmpl-me-command kubectl
  (:inherit-global-flags
   t
   :subcommands
   '("annotate"
     "api-resources"
     "api-versions"
     "apply"
     "attach"
     "auth"
     "autoscale"
     "certificate"
     "cluster-info"
     "completion"
     "config"
     "cordon"
     "cp"
     "create"
     "debug"
     "delete"
     "describe"
     "diff"
     "drain"
     "edit"
     "exec"
     "explain"
     "expose"
     "get"
     "help"
     "kustomize"
     "label"
     "logs"
     "options"
     "patch"
     "plugin"
     "port-forward"
     "proxy"
     "replace"
     "rollout"
     "run"
     "scale"
     "set"
     "taint"
     "top"
     "uncordon"
     "version"
     "wait")))

(pcmpl-me-command (kubectl annotate)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl annotate" pcmpl-kubectl--global-flags))
   '((("--all"
       "--allow-missing-template-keys"
       "--dry-run"
       "--field-manager"
       "--field-manager="
       "--field-selector"
       "--field-selector="
       "--filename"
       "--filename="
       "--kustomize"
       "--kustomize="
       "--list"
       "--local"
       "--output"
       "--output="
       "--overwrite"
       "--record"
       "--recursive"
       "--resource-version"
       "--resource-version="
       "--selector"
       "--selector="
       "--show-managed-fields"
       "--template"
       "--template="
       "-R"
       "-f"
       "-k"
       "-l"
       "-o")))))

(pcmpl-me-command (kubectl api-versions)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl api-versions" pcmpl-kubectl--global-flags))
   'nil))

(pcmpl-me-command (kubectl config)
  (:inherit-global-flags
   t
   :subcommands
   '("current-context"
     "delete-cluster"
     "delete-context"
     "delete-user"
     "get-clusters"
     "get-contexts"
     "get-users"
     "rename-context"
     "set"
     "set-cluster"
     "set-context"
     "set-credentials"
     "unset"
     "use-context"
     "view")
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config" pcmpl-kubectl--global-flags))
   'nil))

(pcmpl-me-command (kubectl config current-context)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config current-context" pcmpl-kubectl--global-flags))
   'nil))

(pcmpl-me-command (kubectl config delete-cluster)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config delete-cluster" pcmpl-kubectl--global-flags))
   'nil
   :subcommands
   (lambda () (pcmpl-kubectl--complete "clusters"))))

(pcmpl-me-command (kubectl config delete-context)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config delete-context" pcmpl-kubectl--global-flags))
   'nil
   :subcommands
   (lambda () (pcmpl-kubectl--complete "context"))))

(pcmpl-me-command (kubectl config use-context)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl config use-context" pcmpl-kubectl--global-flags))
   'nil
   :subcommands
   (lambda () (pcmpl-kubectl--complete "contexts"))))

(pcmpl-me-command (kubectl api-resources)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl api-resources" pcmpl-kubectl--global-flags))
   '((("--api-group"
       "--api-group="
       "--cached"
       "--namespaced"
       "--no-headers"
       "--output"
       "--output="
       "--sort-by"
       "--sort-by="
       "--verbs"
       "--verbs="
       "-o")))))

(pcmpl-me-command (kubectl apply)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply" pcmpl-kubectl--global-flags))
   '((("--all"
       "--allow-missing-template-keys"
       "--field-manager"
       "--field-manager="
       "--force"
       "--force-conflicts"
       "--grace-period"
       "--grace-period="
       "--openapi-patch"
       "--overwrite"
       "--prune"
       "--prune-whitelist"
       "--prune-whitelist="
       "--record"
       "--recursive"
       "--selector"
       "--selector="
       "--server-side"
       "--show-managed-fields"
       "--template"
       "--template="
       "--timeout"
       "--timeout="
       "--validate"
       "--wait"
       "-R"
       "-f"
       "-l"))
     (("-k" "--kustomize" "--kustomize=") . (:dirs))
     (("-f" "--filename" "--filename=") . (:files))
     (("--output" "--output=" "-o") . (:list "json" "yaml" "name" "go-template" "go-template-file" "template" "templatefile" "jsonpath" "jsonpath-as-json" "jsonpath-file"))
     (("--cascade") . (:list "background" "orphan" "foreground"))
     (("--dry-run") . (:list "none" "server" "client")))
   :subcommands
   '("edit-last-applied" "set-last-applied" "view-last-applied")))


(pcmpl-me-command (kubectl apply edit-last-applied)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply edit-last-applied" pcmpl-kubectl--global-flags))
   '((("--allow-missing-template-keys"
       "--field-manager"
       "--field-manager="
       "--record"
       "--recursive"
       "--show-managed-fields"
       "--template"
       "--template="
       "--windows-line-endings"
       "-R"))
     (("-k" "--kustomize" "--kustomize=") . (:dirs))
     (("-f" "--filename" "--filename=") . (:files))
     (("--output" "--output=" "-o") . (:list "json" "yaml" "name" "go-template" "go-template-file" "template" "templatefile" "jsonpath" "jsonpath-as-json" "jsonpath-file")))))

(pcmpl-me-command (kubectl apply set-last-applied)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply set-last-applied" pcmpl-kubectl--global-flags))
   '((("--allow-missing-template-keys"
       "--create-annotation"
       "--dry-run"
       "--show-managed-fields"
       "--template"
       "--template="
       "-f"))
     (("-f" "--filename" "--filename=") . (:files))
     (("--output" "--output=" "-o") . (:list "json" "yaml" "name" "go-template" "go-template-file" "template" "templatefile" "jsonpath" "jsonpath-as-json" "jsonpath-file")))))

(pcmpl-me-command (kubectl attach)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl attach" pcmpl-kubectl--global-flags))
   '(((
       "--pod-running-timeout"
       "--pod-running-timeout="
       "--quiet"
       "--stdin"
       "--tty"
       "-c"
       "-i"
       "-q"
       "-t"))
     ;; TODO support finding containers
     (("--container" "--container=")))))

(pcmpl-me-command (kubectl auth)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl auth" pcmpl-kubectl--global-flags))
   'nil))

(pcmpl-me-command (kubectl auth can-i)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl auth can-i" pcmpl-kubectl--global-flags))
   '((("--all-namespaces"
       "--list"
       "--no-headers"
       "--quiet"
       "--subresource"
       "--subresource="
       "-A"
       "-q")))))

(pcmpl-me-command (kubectl auth reconcile)
  (:inherit-global-flags
   t
   :flags
   ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl auth reconcile" pcmpl-kubectl--global-flags))
   '((("--allow-missing-template-keys"
       "--dry-run"
       "--output"
       "--output="
       "--recursive"
       "--remove-extra-permissions"
       "--remove-extra-subjects"
       "--show-managed-fields"
       "--template"
       "--template="
       "-R"
       "-o"))
     (("-k" "--kustomize" "--kustomize=") . (:dirs))
     (("-f" "--filename" "--filename=") . (:files)))))


;;
;; PComplete kubectl
;;

(defvar pcmpl-kubectl--namespace nil)
(defvar pcmpl-kubectl--context nil)

;;;###autoload
(defun pcomplete/kubectl ()
  "Completion for kubectl."
  (pcmpl-kubectl))

(provide 'pcmpl-kubectl)
;;; pcmpl-kubectl.el ends here
